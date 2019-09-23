{ briareusSrc ? "/home/galois/Briareus"
, briareus_rundir ? "/var/run/briareus"
, thespian_director_dir ? briareus_rundir + "/thespian"
, pkgs ? import <nixpkgs> {}
, briareusPeriod ? 30  # Period between briareus runs for a project (in minutes)
}:

let

  briareus = pkgs.callPackage briareusSrc {};

  briareus_thespian_director = pkgs.callPackage (briareusSrc + "/thespian/director") {};

  briareus_outfile = projectname: "${briareus_rundir}/${projectname}.hhc";

  slash_join = a: b:
    # Joins a and b together with a single slash between them.
    # Handles the cases where a ends with one or more slashes and b
    # begins with one or more slashes; the result will always have
    # only a single slash between a and b.
    with builtins;
    let len_a = stringLength a;
    in if stringLength b == 0
       then a
       else
         (if substring (len_a - 1) 1 a == "/"
          then (if substring 0 1 b == "/"
                then slash_join a (substring 0 1 b)
                else a + b)
          else (if substring 0 1 b == "/"
                then a + b
                else a + "/" + b));

  retrieval_url = base: src_subdir: file:
    # Given the base url, a subdirectory in that url target, returns a
    # full URL to reference the target file at that URL path.  Handles
    # some conversions of recognized URL's like github.com to get the
    # raw file contents instead of the HTML page for that file.
    #
    # The base may also just be a local absolute filepath, in which
    # case the result is an absolute filepath to the target file.
    with builtins;
    let github_base = "https://github.com/";
        len_gh_base = stringLength github_base;
        len_base = stringLength base;
        rem_base = sub len_base len_gh_base;
    in
    if substring 0 1 base == "/"
    then
      ( # This is an absolute path, not a network path
        slash_join (slash_join base src_subdir) file )
    else (if substring 0 len_gh_base base == github_base
          then
            (slash_join
              (slash_join
                (slash_join ("https://raw.githubusercontent.com/" +
                             (substring len_gh_base rem_base base))
                  "/master/")
                src_subdir)
              file)
          else
            (slash_join
              (slash_join
                (slash_join base "/raw/master/") src_subdir) file));

  mkBriareusProject =
    # Called to generate the NixOS Briareus support components for a
    # project.  If multiple projects are supported (projnum != 0) then
    # the check times for the projects will be staggered on 3-minute
    # intervals and a common briareus service is used for for handling
    # the persistent daemon and caching elements.
    projnum:  # null or this project's number
    project:  # attrset that describes the project
    #  project.name  = name string, short, unique, embeddable
    #  project.hhSrc = URL or path to retrieve the hhd and hhb files from
    #  project.hhd   = filename for Briareus input specification (in hhSrc)
    #  project.hhb   = filename for Builder Backend configuration (in hhSrc)

    let inp_hhd = retrieval_url project.hhSrc (project.hhSubdir or "") project.hhd;
        inp_hhb = retrieval_url project.hhSrc (project.hhSubdir or "") project.hhb;

        # This script is run periodically to fetch the input Briareus
        # files for this project (in case they have changed) and
        # re-run briareus to generate new build configurations (which
        # are updated to the Hydra project input location only on
        # changes).  Hydra will then see these changes and update the
        # jobsets for this project automatically.

        run_script = ''
             set -x
             mkdir -p ${briareus_rundir};
             pwd
             cd ${briareus_rundir};
             pwd

             fetch_briareus () {
               case $1 in
                 http*) ${pkgs.curl}/bin/curl -o $2 $1 ;;
                 *)     cp $1 $2 ;;
               esac
             }

             # Fetch Briareus input file updates for this project
             if fetch_briareus ${inp_hhd} ${project.hhd}.new && \
                fetch_briareus ${inp_hhb} ${project.hhb}.new; then
               # Successfully fetched, so install these as the new files to use
               mv ${project.hhd}.new ${project.hhd}
               mv ${project.hhb}.new ${project.hhb}
               # Generate a new project config in case the input files would cause this to change
               cp $(${pkgs.nix}/bin/nix eval --raw "(import ${briareus}/hydra/sysconfig.nix { briareusSrc = ${briareusSrc}; }).mkProjectCfg $(pwd)/${project.hhb}") ${briareus_rundir}/${project.name}-hydra-project-config.json
             fi

             # Run Briareus to generate build configs for Hydra
             ${briareus}/bin/hh -v ${project.hhd} -b hydra -B ${project.hhb} -o ${briareus_outfile project.name}.new
             if [ $? -eq 0 ] && ! ${pkgs.diffutils}/bin/cmp -s ${briareus_outfile project.name} ${briareus_outfile project.name}.new ; then
               cp ${briareus_outfile project.name}.new ${briareus_outfile project.name}
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
      systemd.services."briareus-${project.name}" = {
        description = "Briareus build configuration generator for ${project.name}";
        serviceConfig = {
          Type = "forking";
          TimeoutSec = 300;  # this script can take a while to run if there are lots of PRs and repos.
        };
        script = run_script;
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
    #  project.name  = name string, short, unique, embeddable
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
      environment = { THESPIAN_DIRECTOR_DIR=thespian_director_dir; };
      serviceConfig = {
        Type = "forking";
        ExecStartPre="${pkgs.bash}/bin/bash ${briareus_thespian_director}/install_to ${thespian_director_dir}";
        ExecStart="${pkgs.python37.withPackages (pp: [ pp.thespian pp.setproctitle pp.requests ])}/bin/python -m thespian.director start";
        ExecStartPost="${pkgs.coreutils}/bin/sleep 10";
        ExecStop="${pkgs.python37.withPackages (pp: [ pp.thespian pp.setproctitle pp.requests ])}/bin/python -m thespian.director shutdown";
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
