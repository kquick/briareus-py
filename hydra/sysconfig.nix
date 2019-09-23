{ briareusSrc ? "/home/galois/Briareus"
, pkgs ? import <nixpkgs> {}
}:

rec {

  briareus = pkgs.callPackage briareusSrc {};

  briareus_rundir = "/var/run/briareus";
  briareus_outfile = projectname: "${briareus_rundir}/${projectname}.hhc";

  slash_join = a: b:
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

  mkBriareus =
    # Called to generate the NixOS Briareus support components for a
    # project.
    project:  # attrset that describes the project
    #  project.name  = name string, short, unique, embeddable
    #  project.hhSrc = URL or path to retrieve the hhd and hhb files from
    #  project.hhd   = filename for Briareus input specification (in hhSrc)
    #  project.hhb   = filename for Builder Backend configuration (in hhSrc)

    let inp_hhd = retrieval_url project.hhSrc (project.hhSubdir or "") project.hhd;
        inp_hhb = retrieval_url project.hhSrc (project.hhSubdir or "") project.hhb;

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

             # Fetch Briareus input updates
             if fetch_briareus ${inp_hhd} ${project.hhd}.new && \
                fetch_briareus ${inp_hhb} ${project.hhb}.new; then
               mv ${project.hhd}.new ${project.hhd}
               mv ${project.hhb}.new ${project.hhb}
               here=$(pwd)
               cp $(${pkgs.nix}/bin/nix eval --raw "(import ${briareus}/hydra/sysconfig.nix { briareusSrc = ${briareusSrc}; }).mkProjectCfg $(pwd)/${project.hhb}") ${briareus_rundir}/${project.name}-hydra-project-config.json
             fi

             # Run Briareus to generate build configs for Hydra
             ${briareus}/bin/hh -v ${project.hhd} -b hydra -B ${project.hhb} -o ${briareus_outfile project.name}.new
             if [ $? -eq 0 ] && !${pkgs.diffutils}/bin/cmp -s ${briareus_outfile project.name} ${briareus_outfile project.name}.new ; then
               cp ${briareus_outfile project.name}.new ${briareus_outfile project.name}
             fi
             '';


    in rec {

      # The service defines a systemd service that will run the
      # Briareus project configuration periodically to update the
      # build configurations.
      #
      # The associated mkProjectCfg will read these build
      # configurations to generate the project's .jobset
      # configuration.
      service.systemd.services."briareus-${project.name}" = {
        description = "Briareus build configuration generator for ${project.name}";
        serviceConfig = {
          Type = "forking";
          TimeoutSec = 300;  # this script can take a while to run if there are lots of PRs and repos.
        };
        script = run_script;
        startAt = "*:0/30";  # every 30 minutes
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
