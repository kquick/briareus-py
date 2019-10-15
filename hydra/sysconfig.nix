{ briareusSrc ? "/home/galois/Briareus"
, briareus_rundir ? "/var/run/briareus"
, thespian_director_dir ? briareus_rundir + "/thespian"
, pkgs ? import <nixpkgs> {}
, briareusPeriod ? 10  # Period between briareus runs for a project (in minutes)
, pythonVer ? "python"
, thespian ? pkgs."${pythonVer}Packages".thespian
}:

let

  # The briareus.pat file can contain a string specifying the Personal
  # Access Tokens to be used for specifying the BRIAREUS_PAT
  # environment variable in the system jobs.
  briareus_pat = if builtins.pathExists ./briareus.pat
                 then builtins.readFile ./briareus.pat
                 else "";

  briareus = pkgs.callPackage briareusSrc {
    inherit thespian;
    inherit (pkgs."${pythonVer}Packages") setproctitle attrs requests;
  };

  briareus_thespian_director = pkgs.callPackage (briareusSrc + "/thespian/director") {};

  briareus_outfile = projectname: "${briareus_rundir}/${projectname}.hhc";

  dropExtension =
    # Drops the filename extension (if any).  Only the last extension
    # will be dropped if there are multiple, and any path component is
    # unchanged.
    n:  # filename to drop extension of
    builtins.head (builtins.split "\\." n);

  mkBriareusProject =
    # Called to generate the NixOS Briareus support components for a
    # project.  If multiple projects are supported (projnum != 0) then
    # the check times for the projects will be staggered on 3-minute
    # intervals and a common briareus service is used for for handling
    # the persistent daemon and caching elements.
    projnum:  # null or this project's number
    project:  # attrset that describes the project
    #  project.hhSrc = URL or path to retrieve the hhd and hhb files from
    #  project.hhd   = filename for Briareus input specification (in hhSrc)
    #  project.hhb   = filename for Builder Backend configuration (in hhSrc)

    let inp_upd = project.hhSrc + "+" + (project.hhSubdir or "");
        name = dropExtension project.hhd;

        # This script is run periodically to fetch the input Briareus
        # files for this project (in case they have changed) and
        # re-run briareus to generate new build configurations (which
        # are updated to the Hydra project input location only on
        # changes).  Hydra will then see these changes and update the
        # jobsets for this project automatically.

        # run_script notes:
        #
        # Assumes replace_json_if_newer oldfile ($2) is in writeable
        # location, but newfile ($1) may not be: specifically it may
        # be in the /nix/store (see nix eval below) and so it must be
        # copied instead of moved in case it is (a) on a different
        # filesystem and (b) the source filesystem is read-only
        # (e.g. /nix/store)

        run_script = ''
             set -x
             mkdir -p ${briareus_rundir};
             cd ${briareus_rundir};

             replace_json_if_newer () { # $1 = newfile, $2 = oldfile
               if ! ${pkgs.diffutils}/bin/cmp -s <(cat $2 | ${pkgs.jq}/bin/jq -S) <(cat $1 | ${pkgs.jq}/bin/jq -S)
               then
                 mv $2 $2.old
                 cp $1 $2
               fi
             }

             # Generate a new project config in case the input files would cause this to change
             if [ -r $(pwd)/${project.hhb} ]
             then
               newprojcfg=$(${pkgs.nix}/bin/nix eval --raw "(import ${briareus}/hydra/sysconfig.nix { briareusSrc = ${briareusSrc}; }).mkProjectCfg $(pwd)/${project.hhb}")
               replace_json_if_newer $newprojcfg ${briareus_rundir}/${name}-hydra-project-config.json
             fi

             # Run Briareus to generate build configs for Hydra
             ${briareus}/bin/hh -v ${project.hhd} -b hydra -B ${project.hhb} -o ${briareus_outfile name}.new -I ${inp_upd}
             if [ $? -eq 0 ] ; then
               replace_json_if_newer ${briareus_outfile name}.new ${briareus_outfile name}
             fi
             '';

        startMin = if projnum == null then 0 else projnum;

    in rec {

      # The service defines a systemd service that will run the
      # Briareus project configuration periodically to update the
      # build configurations.
      #
      # The associated mkProjectCfg will read these build
      # configurations to generate the project's .jobset
      # configuration.
      systemd.services."briareus-${name}" = {
        description = "Briareus build configuration generator for ${name}";
        serviceConfig = {
          Type = "forking";
          TimeoutSec = 300;  # this script can take a while to run if there are lots of PRs and repos.
        };
        script = run_script;
        environment = {
          THESPIAN_DIRECTOR_DIR=thespian_director_dir;
          BRIAREUS_PAT=briareus_pat;
        };
        startAt = "*:${builtins.toString startMin}/${builtins.toString briareusPeriod}";
      } //
      (if projnum == null then {} else {
        wants = [ "briareus.service" ];
        after = [ "briareus.service" ];
      });
    };

  mapEnum = f: l:
    let pnums = builtins.genList (n: n) (builtins.length l);
        callNum = n: let v = builtins.elemAt l n; in f n v;
    in builtins.map callNum pnums;

in
rec {

  mkBriareus =
    # Called to generate the NixOS Briareus support components for a
    # project.
    project:  # attrset that describes the project
    #  project.hhSrc = URL or path to retrieve the hhd and hhb files from
    #  project.hhd   = filename for Briareus input specification (in hhSrc)
    #  project.hhb   = filename for Builder Backend configuration (in hhSrc)
    mkBriareusProject null project;

  mkBriareusMulti =
    # mkBriareusMulti should be used instead of calling mkBriareus
    # multiple times (i.e. when there are multiple Briareus-managed
    # projects).  The mkBriareusMulti ensures that the briareus base
    # service is used to manage the persistent daemon processes and
    # cache.
    projectList:
      [briareusServiceBase] ++ (mapEnum mkBriareusProject projectList);

  briareusServiceBase = {
    systemd.services."briareus" = {
      description = "Briareus central support";
      after = [ "network-online.target" ];
      environment = {
        THESPIAN_DIRECTOR_DIR=thespian_director_dir;
        BRIAREUS_PAT=briareus_pat;
      };
      path = [ pkgs.swiProlog ];
      serviceConfig = {
        Type = "forking";
        ExecStartPre="${pkgs.bash}/bin/bash ${briareus_thespian_director}/install_to ${thespian_director_dir}";
        ExecStart="${pkgs.${pythonVer}.withPackages (pp: [ thespian pp.setproctitle pp.requests ])}/bin/python -m thespian.director start";
        ExecStartPost="${pkgs.coreutils}/bin/sleep 10";
        ExecStop="${pkgs.${pythonVer}.withPackages (pp: [ thespian ])}/bin/python -m thespian.director shutdown";
      };
    };
  };

  mkProjectCfg =
    # mkProjectCfg generates the Hydra declarative project file.  This
    # does not need to be used, and the declarative config can be
    # supplied by other means, but this is a convenience to get a
    # config that is compatible with importing Briareus build_configs.
    inpfile:  # Specifies the Briareus Hydra configuration
    #  inpfile.name = a name given to this project (used for printing only)
    #  inpfile.project = project-specific overrides
    let
      projcfg_generic = name: {
        checkinterval = 300;
        schedulingshares = 1;
        emailoverride = "";
        keepnr = 3;
        description = "Briareus-generated ${name} Project declaration";
        nixexprinput = "briareus_src";
        nixexprpath = "hydra/copy_hh.nix";
        enabled = 1;
        hidden = false;
        enableemail = true;
        inputs = {
          nixpkgs = {
            type = "git";
            value = "https://github.com/NixOS/nixpkgs-channels nixos-unstable";
            emailresponsible = false;
          };
          hh_output = {
            type = "path";
            value = briareus_outfile name;
            emailresponsible = false;
          };
          briareus_src = {
            type = "path";
            value = briareusSrc;
            emailresponsible = false;
          };
        };
      };

      projcfg_overrides = lclhhb:
        builtins.fromJSON (builtins.readFile "${lclhhb}");

      projcfg_data = name: lclcfg:
        let gencfg = projcfg_generic name;
        in gencfg // lclcfg // { inputs = gencfg.inputs // (lclcfg.inputs or {}); };

      projcfg_file = name: lclcfg:
        builtins.toFile "${name}-hydra-project-config.json"
        (builtins.toJSON (projcfg_data name lclcfg));

      cfg = projcfg_overrides inpfile;

    in projcfg_file (cfg.name or "project") (cfg.project or {});
}
