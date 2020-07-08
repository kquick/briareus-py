from Briareus.Types import (BldConfig, BldRepoRev, BldVariable,
                            ProjectSummary, StatusReport, VarFailure,
                            PR_Grouped, BranchReq, MainBranch,
                            PRCfg, BranchCfg,
                            PRData, PRFailedSubBlds, BldSet,
                            Notify,
                            SendEmail, SetForgeStatus)
import Briareus.hh as hh
from Briareus.VCS.ForgeAccess import RepoAPI_Location
from test_example import expected_repo_info
import json
import pytest
from unittest.mock import (patch, ANY, call)
from datetime import timedelta
from thespian.actors import ActorSystem


input_spec = open('test/inp_example').read()

build_names = [
    "PR-blah.HEADs-clang-ghc844",  # 1 11 111 1111
    "PR-blah.HEADs-gnucc-ghc844",
    # "PR-blah.HEADs-clang-ghc865",
    "PR-blah.HEADs-gnucc-ghc865",
    # "PR-blah.HEADs-clang-ghc881",
    "PR-blah.HEADs-gnucc-ghc881",
    "PR-blah.submodules-clang-ghc844", # 1 111
    "PR-blah.submodules-gnucc-ghc844",
    # "PR-blah.submodules-clang-ghc865",
    "PR-blah.submodules-gnucc-ghc865",
    # "PR-blah.submodules-clang-ghc881",
    "PR-blah.submodules-gnucc-ghc881",
    "PR-bugfix9.HEADs-clang-ghc844",  # 23 8192
    "PR-bugfix9.HEADs-gnucc-ghc844",
    # "PR-bugfix9.HEADs-clang-ghc865",
    "PR-bugfix9.HEADs-gnucc-ghc865",
    # "PR-bugfix9.HEADs-clang-ghc881",
    "PR-bugfix9.HEADs-gnucc-ghc881",
    "PR-bugfix9.submodules-clang-ghc844",
    "PR-bugfix9.submodules-gnucc-ghc844",
    # "PR-bugfix9.submodules-clang-ghc865",
    "PR-bugfix9.submodules-gnucc-ghc865",
    # "PR-bugfix9.submodules-clang-ghc881",
    "PR-bugfix9.submodules-gnucc-ghc881",
    "dev.HEADs-clang-ghc844",
    "dev.HEADs-gnucc-ghc844",
    # "dev.HEADs-clang-ghc865",
    "dev.HEADs-gnucc-ghc865",
    # "dev.HEADs-clang-ghc881",
    "dev.HEADs-gnucc-ghc881",
    "dev.submodules-clang-ghc844",
    "dev.submodules-gnucc-ghc844",
    # "dev.submodules-clang-ghc865",
    "dev.submodules-gnucc-ghc865",
    # "dev.submodules-clang-ghc881",
    "dev.submodules-gnucc-ghc881",
    "feat1.HEADs-clang-ghc844",
    "feat1.HEADs-gnucc-ghc844",
    # "feat1.HEADs-clang-ghc865",
    "feat1.HEADs-gnucc-ghc865",
    # "feat1.HEADs-clang-ghc881",
    "feat1.HEADs-gnucc-ghc881",
    "feat1.submodules-clang-ghc844",
    "feat1.submodules-gnucc-ghc844",
    # "feat1.submodules-clang-ghc865",
    "feat1.submodules-gnucc-ghc865",
    # "feat1.submodules-clang-ghc881",
    "feat1.submodules-gnucc-ghc881",
    "master.HEADs-clang-ghc844",
    "master.HEADs-gnucc-ghc844",
    # "master.HEADs-clang-ghc865",
    "master.HEADs-gnucc-ghc865",
    # "master.HEADs-clang-ghc881",
    "master.HEADs-gnucc-ghc881",
    "master.submodules-clang-ghc844",
    "master.submodules-gnucc-ghc844",
    # "master.submodules-clang-ghc865",
    "master.submodules-gnucc-ghc865",
    # "master.submodules-clang-ghc881",
    "master.submodules-gnucc-ghc881",
]

build_results = [
    { "name": n,
      "nrtotal" : 10,
      "nrsucceeded": 8 if '-clang-' in n else 10,
      "nrfailed": 2 if '-clang-' in n else 0,
      "nrscheduled": 0,
      "haserrormsg": False,
      "fetcherrormsg": '',
    }
    for n in build_names
]

prior = [
    StatusReport(status='initial_success', project='Project #1',
                 strategy="submodules", branchtype="regular", branch="master",
                 buildname='master.submodules-gnucc-ghc844',
                 bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue='ghc844'),
                          BldVariable(project='Project #1', varname='c_compiler', varvalue='gnucc'),
                 ],
                 blddesc=BranchReq('Project #1', 'master'),
    ),
    StatusReport(status=2, project='Project #1',
                 strategy="HEADs", branchtype="regular", branch="master",
                 buildname='master.HEADs-gnucc-ghc865',
                 bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue='ghc865'),
                          BldVariable(project='Project #1', varname='c_compiler', varvalue='gnucc'),
                 ],
                 blddesc=BranchReq('Project #1', 'master'),
    ),
    StatusReport(status='succeeded', project='Project #1',
                 strategy="HEADs", branchtype="regular", branch="master",
                 buildname='master.HEADs-gnucc-ghc844',
                 bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue='ghc844'),
                          BldVariable(project='Project #1', varname='c_compiler', varvalue='gnucc'),
                 ],
                 blddesc=BranchReq('Project #1', 'master'),
    ),
    SendEmail(recipients=['eddy@nocompany.com'],
              notification=Notify(what='main_submodules_good', subject='Project #1', params='master'),
              sent_to=['eddy@nocompany.com']),
]

analysis_time_budget = timedelta(seconds=1, milliseconds=750)  # avg 1.06s

@pytest.fixture(scope="module")
def example_hydra_results(generate_hydra_results):
    r = generate_hydra_results(build_results=build_results, prior=prior)
    return r[0], r[1].report


def test_example_report_summary(example_hydra_results):
    bldcfgs, reps = example_hydra_results

    for each in reps:
        print('')
        print(each)
    print('')
    print(len(reps))
    assert ProjectSummary(project_name='Project #1',
                          bldcfg_count=40, subrepo_count=4, pullreq_count=6) in reps

def test_example_report_status1(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for a single entry
    assert StatusReport(status=2, project='Project #1',
                        strategy="HEADs", branchtype="pullreq", branch="blah",
                        buildname='PR-blah.HEADs-clang-ghc844',
                        bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue='ghc844'),
                                 BldVariable(project='Project #1', varname='c_compiler', varvalue='clang'),
                        ],
                        blddesc=PR_Grouped('blah'),
    ) in reps

CS = [ 'gnucc', 'clang' ]
GS = [ 'ghc844', 'ghc865', 'ghc881' ]
SS = [ 'HEADs', 'submodules' ]
BS = [ 'PR-bugfix9',  # PR23-PR8192
       "feat1", "master", "dev",]

def test_example_report_statusMany(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for all entries that should be present
    for G in GS:
        for C in CS if G == GS[0] else CS[:1]:
            for S in SS:
                for B in BS:
                    r = StatusReport(
                        status=('succeeded'
                                if C == 'gnucc' and G == 'ghc844' and S == 'submodules' and B == 'master'
                                else 'fixed'
                                if C == 'gnucc' and G == 'ghc865' and S == 'HEADs' and B == 'master'
                                else 'succeeded'
                                if C == 'gnucc' and G == 'ghc844' and S == 'HEADs' and B == 'master'
                                else 2
                                if C == 'clang'
                                else 'initial_success'),
                        project='Project #1',
                        strategy=S,
                        branchtype="pullreq" if B.startswith('PR') else "regular",
                        branch=B.split('-')[-1] if B.startswith('PR') else B,
                        buildname='-'.join(['.'.join([B,S]),C,G]),
                        bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue=G),
                                 BldVariable(project='Project #1', varname='c_compiler', varvalue=C),
                        ],
                        blddesc=(PR_Grouped(B[3:]) if B.startswith('PR-') else
                                 BranchReq('Project #1', B)),
                    )
                    assert r in reps

def test_example_report_status2(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for all entries that should be present
    for G in GS:
        for C in CS if G == GS[0] else CS[:1]:
            S = 'submodules'
            B = 'PR-blah' # PR1-PR111-blah
            r = StatusReport(
                status=(2
                        if C == 'clang'
                        else 'initial_success'),
                project='Project #1',
                strategy=S,
                branchtype="pullreq" if B.startswith('PR') else "regular",
                branch=B.split('-')[-1] if B.startswith('PR') else B,
                buildname='-'.join(['.'.join([B,S]),C,G]),
                bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue=G),
                         BldVariable(project='Project #1', varname='c_compiler', varvalue=C),
                ],
                blddesc=PR_Grouped(B[3:]) if B.startswith('PR-') else (
                    MainBranch('R1', B) if B == 'master' else
                    BranchReq('Project #1', B)),
            )
            assert r in reps

def test_example_report_status3(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Check for all entries that should be present
    for G in GS:
        for C in CS if G == GS[0] else CS[:1]:
            S = 'HEADs'
            B = 'PR-blah' # PR1-PR11-PR111-PR1111-blah
            r = StatusReport(
                status=(2
                        if C == 'clang'
                        else 'initial_success'),
                project='Project #1',
                strategy=S,
                branchtype="pullreq" if B.startswith('PR') else "regular",
                branch=B.split('-')[-1] if B.startswith('PR') else B,
                buildname='-'.join(['.'.join([B,S]),C,G]),
                bldvars=[BldVariable(project='Project #1', varname='ghcver', varvalue=G),
                         BldVariable(project='Project #1', varname='c_compiler', varvalue=C),
                ],
                blddesc=PR_Grouped(B[3:]) if B.startswith('PR-') else (
                    MainBranch('R1', B) if B == 'master' else
                    BranchReq('Project #1', B)),
            )
            assert r in reps

def test_example_report_varfailure(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    assert VarFailure('Project #1', 'c_compiler', 'clang') in reps

def test_example_report_length(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    # Verify that there are no unexpected additional entries
    additional_bldcfgs = 1 # status2 + status3
    num_varfailure = 1
    pr_status = 2
    num_notify = num_varfailure + 1 + pr_status # 1 = main_submodules_good
    num_actions = (num_varfailure +
                   1 + # main_submodules_good
                   pr_status +  # SendEmail
                   pr_status    # SetForgeStatus
                   )
    expected = (((len(CS) + len(GS) - 1) * len(SS) * (len(BS)+additional_bldcfgs)) +
                len(['ProjectSummary'])
                + (num_varfailure * 2)  # VarValue + SepHandledVar
                + (pr_status * 2) # HEADs and submodules
                + num_notify
                + num_actions)
    for each in reps:
        print(each)
        print()
    print(len(CS),len(GS),len(SS),len(BS))  # 2 3 2 4
    assert expected == len(reps)


# ----------------------------------------------------------------------

def test_example_report_varfail_do_email(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    print('')
    print(len(reps))
    assert SendEmail(recipients=sorted(['eddy@nocompany.com',
                                        'fred@nocompany.com',
                                        'betty@nocompany.com',
                                        'john@_company.com',
                                        'sam@not_a_company.com']),
                     notification=Notify(what='variable_failing',
                                         subject='Project #1',
                                         params=BldVariable(project='Project #1',
                                                            varname='c_compiler',
                                                            varvalue='clang')),
                     sent_to=[]) in reps

def test_example_report_varfail_do_email_again(generate_hydra_results):
    """Express a prior send of an email to the target; this ensures that
       these prior sends are retained.
    """
    bldcfgs, ctxt = generate_hydra_results(
        build_results=build_results,
        prior=prior + [
            SendEmail(recipients=['fred@nocompany.com'],
                      notification=Notify(what='variable_failing',
                                          subject='Project #1',
                                          params=BldVariable(project='Project #1',
                                                             varname='c_compiler',
                                                             varvalue='clang')),
                      sent_to=['fred@nocompany.com'])
        ],
    )
    reps = ctxt.report

    recipients = sorted(['eddy@nocompany.com',
                         'fred@nocompany.com',
                         'john@_company.com',
                         'sam@not_a_company.com',
                         'betty@nocompany.com'])
    assert SendEmail(recipients=recipients,
                     notification=Notify(what='variable_failing',
                                         subject='Project #1',
                                         params=BldVariable(project='Project #1',
                                                            varname='c_compiler',
                                                            varvalue='clang')),
                     sent_to=[]) not in reps
    assert SendEmail(recipients=recipients,
                     notification=Notify(what='variable_failing',
                                         subject='Project #1',
                                         params=BldVariable(project='Project #1',
                                                            varname='c_compiler',
                                                            varvalue='clang')),
                     sent_to=['fred@nocompany.com']) in reps

def test_pr_projstatus_fail_do_email(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    print('')
    print(len(reps))
    for each in reps:
        if isinstance(each, SendEmail):
            print(str(each))
            print('')
    assert SendEmail(
        recipients=sorted(['eddy@nocompany.com',
                           'fred@nocompany.com',
                           'john@_company.com',
                           'sam@not_a_company.com']),
        notification=Notify(
            what='pr_projstatus_fail',
            subject='Project #1',
            params=PRFailedSubBlds(
                PR_Grouped('blah'),
                [PRCfg('R1', '1', 'blah', 'r1_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R2', '1111', 'blah', 'r2_blah_mergeref', 'not_nick', 'not_nick@bad.seeds'),
                 PRCfg('R3', '11', 'blah', 'r3_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R6', '111', 'blah', 'r6_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 BranchCfg('R5', 'blah'),
                ],
                BldSet('pullreq', 'submodules',
                       goods=['PR-blah.submodules-gnucc-ghc844',
                              'PR-blah.submodules-gnucc-ghc865',
                              'PR-blah.submodules-gnucc-ghc881',
                       ],
                       fails=['PR-blah.submodules-clang-ghc844',
                              # 'PR-blah.submodules-clang-ghc865',
                              # 'PR-blah.submodules-clang-ghc881',
                       ]),
                BldSet('pullreq', 'HEADs',
                       goods=['PR-blah.HEADs-gnucc-ghc844',
                              'PR-blah.HEADs-gnucc-ghc865',
                              'PR-blah.HEADs-gnucc-ghc881',
                       ],
                       fails=['PR-blah.HEADs-clang-ghc844',
                              # 'PR-blah.HEADs-clang-ghc865',
                              # 'PR-blah.HEADs-clang-ghc881',
                       ]),
                BldSet('regular', 'HEADs',
                       goods=['master.HEADs-gnucc-ghc844',
                              'master.HEADs-gnucc-ghc865',
                              'master.HEADs-gnucc-ghc881',
                       ],
                       fails=['master.HEADs-clang-ghc844',
                              # 'master.HEADs-clang-ghc865',
                              # 'master.HEADs-clang-ghc881',
                       ],
                       ),
                BldSet('regular', 'submodules',
                       goods=['master.submodules-gnucc-ghc844',
                              'master.submodules-gnucc-ghc865',
                              'master.submodules-gnucc-ghc881',
                       ],
                       fails=['master.submodules-clang-ghc844',
                              # 'master.submodules-clang-ghc865',
                              # 'master.submodules-clang-ghc881',
                       ],
                ),
            )),
        sent_to=[]) in reps

def test_pr_projstatus_fail_do_set_forge_status(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    print('')
    print(len(reps))
    for each in reps:
        if isinstance(each, SendEmail):
            print(str(each))
            print('')
    assert SendEmail(
        recipients=sorted(['eddy@nocompany.com',
                           'fred@nocompany.com',
                           'john@_company.com',
                           'sam@not_a_company.com']),
        notification=Notify(
            what='pr_projstatus_fail',
            subject='Project #1',
            params=PRFailedSubBlds(
                PR_Grouped('blah'),
                [PRCfg('R1', '1', 'blah', 'r1_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R2', '1111', 'blah', 'r2_blah_mergeref', 'not_nick', 'not_nick@bad.seeds'),
                 PRCfg('R3', '11', 'blah', 'r3_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R6', '111', 'blah', 'r6_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 BranchCfg('R5', 'blah'),
                ],
                BldSet('pullreq', 'submodules',
                       goods=['PR-blah.submodules-gnucc-ghc844',
                              'PR-blah.submodules-gnucc-ghc865',
                              'PR-blah.submodules-gnucc-ghc881',
                       ],
                       fails=['PR-blah.submodules-clang-ghc844',
                              # 'PR-blah.submodules-clang-ghc865',
                              # 'PR-blah.submodules-clang-ghc881',
                       ],
                ),
                BldSet('pullreq', 'HEADs',
                       goods=['PR-blah.HEADs-gnucc-ghc844',
                              'PR-blah.HEADs-gnucc-ghc865',
                              'PR-blah.HEADs-gnucc-ghc881',
                       ],
                       fails=['PR-blah.HEADs-clang-ghc844',
                              # 'PR-blah.HEADs-clang-ghc865',
                              # 'PR-blah.HEADs-clang-ghc881',
                       ],
                ),
                BldSet('regular', 'HEADs',
                       goods=['master.HEADs-gnucc-ghc844',
                              'master.HEADs-gnucc-ghc865',
                              'master.HEADs-gnucc-ghc881',
                       ],
                       fails=['master.HEADs-clang-ghc844',
                              # 'master.HEADs-clang-ghc865',
                              # 'master.HEADs-clang-ghc881',
                       ],
                ),
                BldSet('regular', 'submodules',
                       goods=['master.submodules-gnucc-ghc844',
                              'master.submodules-gnucc-ghc865',
                              'master.submodules-gnucc-ghc881',
                       ],
                       fails=['master.submodules-clang-ghc844',
                              # 'master.submodules-clang-ghc865',
                              # 'master.submodules-clang-ghc881',
                       ],
                ),
            )),
        sent_to=[]) in reps


# ----------------------------------------------------------------------

bugfix9_prtype = PR_Grouped('bugfix9')
bugfix9_prcfg = [ PRCfg('R2', '23', 'bugfix9', 'r2_b9_mergeref', 'banana', ''),
                  PRCfg('R4', '8192', 'bugfix9', 'r4_bf9_mergeref', 'ozzie', 'ozzie@crazy.train'),
                  BranchCfg('R5', 'bugfix9'),
]

bugfix9_prfaildata=PRFailedSubBlds(
    bugfix9_prtype,
    bugfix9_prcfg,
    BldSet('pullreq', 'submodules',
           goods=['PR-bugfix9.submodules-gnucc-ghc844',
                  'PR-bugfix9.submodules-gnucc-ghc865',
                  'PR-bugfix9.submodules-gnucc-ghc881',
           ],
           fails=['PR-bugfix9.submodules-clang-ghc844',
                  # 'PR-bugfix9.submodules-clang-ghc865',
                  # 'PR-bugfix9.submodules-clang-ghc881',
           ],
    ),
    BldSet('pullreq', 'HEADs',
           goods=['PR-bugfix9.HEADs-gnucc-ghc844',
                  'PR-bugfix9.HEADs-gnucc-ghc865',
                  'PR-bugfix9.HEADs-gnucc-ghc881',
           ],
           fails=['PR-bugfix9.HEADs-clang-ghc844',
                  # 'PR-bugfix9.HEADs-clang-ghc865',
                  # 'PR-bugfix9.HEADs-clang-ghc881',
           ],
    ),
    BldSet('regular', 'HEADs',
           goods=['master.HEADs-gnucc-ghc844',
                  'master.HEADs-gnucc-ghc865',
                  'master.HEADs-gnucc-ghc881',
           ],
           fails=['master.HEADs-clang-ghc844',
                  # 'master.HEADs-clang-ghc865',
                  # 'master.HEADs-clang-ghc881',
           ],
    ),
    BldSet('regular', 'submodules',
           goods=['master.submodules-gnucc-ghc844',
                  'master.submodules-gnucc-ghc865',
                  'master.submodules-gnucc-ghc881',
           ],
           fails=['master.submodules-clang-ghc844',
                  # 'master.submodules-clang-ghc865',
                  # 'master.submodules-clang-ghc881',
           ],
    ),
)

@patch('Briareus.Actions.Actors.SetForgeStatus.GitForgeStatus')
@patch('Briareus.Actions.Actors.SetForgeStatus.os.getenv')
def test_pr_bugfix9_fail_do_first_setforgestatus(getenv, gitforge, generate_hydra_results):
    asys = ActorSystem('simpleSystemBase')
    getenv.side_effect = lambda var, defval=None: "1" if var == 'BRIAREUS_FORGE_STATUS' else defval

    bldcfgs, ctxt = generate_hydra_results(
        build_results=build_results,
        prior=prior)
    rctxt = hh.perform_hh_actions([input_spec], ctxt.report, ctxt, dict())
    reps = rctxt.report
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_fail',
                              subject='Project #1',
                              params=bugfix9_prfaildata),
                          updated=["R2", "R4"]) in reps

    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r2_url", apitoken=None))
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r4_url", apitoken=None))
    gitforge().set_commit_sts.assert_has_calls(
        [
            call('failure',
                 'Fails 1/1/4 (master 1/1/4) submods/heads/total',
                 ref,
                 'http://hydra.builder/path/project/Project #1',
                 'Project #1')
            for ref in [
                    'r2_b9_mergeref',
                    'r4_bf9_mergeref',
            ]
        ],
        any_order=True)

@patch('Briareus.Actions.Actors.SetForgeStatus.GitForgeStatus')
@patch('Briareus.Actions.Actors.SetForgeStatus.os.getenv')
def test_pr_bugfix9_fail_do_again_setforgestatus(getenv, gitforge, generate_hydra_results):
    asys = ActorSystem('simpleSystemBase')
    getenv.side_effect = lambda var, defval=None: "1" if var == 'BRIAREUS_FORGE_STATUS' else defval

    bldcfgs, ctxt = generate_hydra_results(
        build_results=build_results,
        prior=prior + [
            SetForgeStatus(targetrepos=["R2", "R4"],
                           notification=Notify(what='pr_projstatus_fail',
                                               subject='Project #1',
                                               params=bugfix9_prfaildata),
                           updated=['R2'])],
        )
    rctxt = hh.perform_hh_actions([input_spec], ctxt.report, ctxt, dict())
    reps = rctxt.report
    print('')
    print(len(reps))
    for each in reps:
        if isinstance(each, SetForgeStatus):
            print(str(each))
            print('')
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_fail',
                              subject='Project #1',
                              params=bugfix9_prfaildata),
                          updated=["R2", "R4"]) in reps

    # print(gitforge.call_args_list)
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r2_url", apitoken=None))
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r4_url", apitoken=None))
    # print(gitforge().set_commit_status.call_args_list)
    gitforge().set_commit_sts.assert_has_calls(
        [
            call('failure',
                 'Fails 1/1/4 (master 1/1/4) submods/heads/total',
                 ref,
                 'http://hydra.builder/path/project/Project #1',
                 'Project #1')
            for ref in [
                    # 'r2_b9_mergeref',  # This is in a prior, so no notification this time
                    'r4_bf9_mergeref',   # OK
            ]
        ],
        any_order=True)

@patch('Briareus.Actions.Actors.SetForgeStatus.GitForgeStatus')
@patch('Briareus.Actions.Actors.SetForgeStatus.os.getenv')
def test_pr_bugfix9_fail_do_alldone_setforgestatus(getenv, gitforge, generate_hydra_results):
    asys = ActorSystem('simpleSystemBase')
    getenv.side_effect = lambda var, defval=None: "1" if var == 'BRIAREUS_FORGE_STATUS' else defval

    bldcfgs, ctxt = generate_hydra_results(
        build_results=build_results,
        prior=prior + [
            SetForgeStatus(targetrepos=["R2", "R4"],
                           notification=Notify(what='pr_projstatus_fail',
                                               subject='Project #1',
                                               params=bugfix9_prfaildata),
                           updated=['R2', 'R4'])],
        )
    rctxt = hh.perform_hh_actions([input_spec], ctxt.report, ctxt, dict())
    reps = rctxt.report
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_fail',
                              subject='Project #1',
                              params=bugfix9_prfaildata),
                          updated=["R2", "R4"]) in reps

    for each in gitforge().set_commit_sts.call_args_list:
        assert each.ref not in ['r2_b9_mergeref', 'r4_bf9_patch' ]

@patch('Briareus.Actions.Actors.SetForgeStatus.GitForgeStatus')
@patch('Briareus.Actions.Actors.SetForgeStatus.os.getenv')
def test_pr_bugfix9_fail_supplement_setforgestatus(getenv, gitforge, generate_hydra_results):
    asys = ActorSystem('simpleSystemBase')
    getenv.side_effect = lambda var, defval=None: "1" if var == 'BRIAREUS_FORGE_STATUS' else defval

    bldcfgs, ctxt = generate_hydra_results(
        build_results=build_results,
        prior=prior)
    rctxt = hh.perform_hh_actions([input_spec], ctxt.report, ctxt,
                                  { 'status_url': 'https://company.com/base/{project}/status',
                                  })
    reps = rctxt.report
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_fail',
                              subject='Project #1',
                              params=bugfix9_prfaildata),
                          updated=["R2", "R4"]) in reps

    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r2_url", apitoken=None))
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r4_url", apitoken=None))
    gitforge().set_commit_sts.assert_has_calls(
        [
            call('failure',
                 'Fails 1/1/4 (master 1/1/4) submods/heads/total',
                 ref,
                 'https://company.com/base/Project #1/status',
                 'Project #1')
            for ref in [
                    'r2_b9_mergeref',
                    'r4_bf9_mergeref',
            ]
        ],
        any_order=True)

@patch('Briareus.Actions.Actors.SetForgeStatus.GitForgeStatus')
@patch('Briareus.Actions.Actors.SetForgeStatus.os.getenv')
def test_pr_bugfix9_only_first_pending_setforgestatus(getenv, gitforge, generate_hydra_results):
    asys = ActorSystem('simpleSystemBase')
    getenv.side_effect = lambda var, defval=None: "1" if var == 'BRIAREUS_FORGE_STATUS' else defval

    bldcfgs, ctxt = generate_hydra_results(
        build_results=[
            { "name": n,
              "nrtotal" : 10,
              "nrsucceeded": 5,
              "nrfailed": 0,
              "nrscheduled": 5,
              "haserrormsg": False,
              "fetcherrormsg": "",
            }
            for n in build_names],
        prior=[])
    rctxt = hh.perform_hh_actions([input_spec], ctxt.report, ctxt, dict())
    reps = rctxt.report
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_pending',
                              subject='Project #1',
                              params=PRData(bugfix9_prtype, bugfix9_prcfg)),
                          updated=["R2", "R4"]) in reps

    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r2_url", apitoken=None))
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r4_url", apitoken=None))
    # print(gitforge().set_commit_status.call_args_list)
    gitforge().set_commit_sts.assert_has_calls(
        [
            call('pending',
                 'Build of Project #1 is pending...\n', ref,
                 'http://hydra.builder/path/project/Project #1',
                 'Project #1')
            for ref in [
                    'r2_b9_mergeref',
                    'r4_bf9_mergeref',
            ]
        ],
        any_order=True)

    gitforge().reset_mock()

    bldcfgs2, ctxt2 = generate_hydra_results(
        build_results=[
            { "name": n,
              "nrtotal" : 10,
              "nrsucceeded": 8,
              "nrfailed": 0,
              "nrscheduled": 2,
              "haserrormsg": False,
              "fetcherrormsg": "",
            }
            for n in build_names],
        prior=[x for x in reps if isinstance(x, SetForgeStatus)])
    rctxt2 = hh.perform_hh_actions([input_spec], ctxt2.report, ctxt2, dict())
    reps = rctxt2.report
    assert SetForgeStatus(targetrepos=["R2", "R4"],
                          notification=Notify(
                              what='pr_projstatus_pending',
                              subject='Project #1',
                              params=PRData(bugfix9_prtype, bugfix9_prcfg)),
                          updated=["R2", "R4"]) in reps

    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r2_url", apitoken=None))
    gitforge.assert_any_call(RepoAPI_Location(apiloc="https://github.com/r4_url", apitoken=None))
    # Ensure the pending status is not re-asserted, even if the number pending changes
    assert gitforge().set_commit_sts.call_args_list == []


def test_pr_blah_fail_do_setforgestatus(example_hydra_results):
    bldcfgs, reps = example_hydra_results
    print('')
    print(len(reps))
    for each in reps:
        if isinstance(each, SetForgeStatus):
            print(str(each))
            print('')
    assert SetForgeStatus(
        targetrepos=["R1", "R2", "R3", "R6"],
        notification=Notify(
            what='pr_projstatus_fail',
            subject='Project #1',
            params=PRFailedSubBlds(
                PR_Grouped('blah'),
                [PRCfg('R1', '1', 'blah', 'r1_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R2', '1111', 'blah', 'r2_blah_mergeref', 'not_nick', 'not_nick@bad.seeds'),
                 PRCfg('R3', '11', 'blah', 'r3_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 PRCfg('R6', '111', 'blah', 'r6_blah_mergeref', 'nick', 'nick@bad.seeds'),
                 BranchCfg('R5', 'blah'),
                ],
                BldSet('pullreq', 'submodules',
                       goods=['PR-blah.submodules-gnucc-ghc844',
                              'PR-blah.submodules-gnucc-ghc865',
                              'PR-blah.submodules-gnucc-ghc881',
                       ],
                       fails=['PR-blah.submodules-clang-ghc844',
                              # 'PR-blah.submodules-clang-ghc865',
                              # 'PR-blah.submodules-clang-ghc881',
                       ],
                ),
                BldSet('pullreq', 'HEADs',
                       goods=['PR-blah.HEADs-gnucc-ghc844',
                              'PR-blah.HEADs-gnucc-ghc865',
                              'PR-blah.HEADs-gnucc-ghc881',
                       ],
                       fails=['PR-blah.HEADs-clang-ghc844',
                              # 'PR-blah.HEADs-clang-ghc865',
                              # 'PR-blah.HEADs-clang-ghc881',
                       ],
                ),
                BldSet('regular', 'HEADs',
                       goods=['master.HEADs-gnucc-ghc844',
                              'master.HEADs-gnucc-ghc865',
                              'master.HEADs-gnucc-ghc881',
                       ],
                       fails=['master.HEADs-clang-ghc844',
                              # 'master.HEADs-clang-ghc865',
                              # 'master.HEADs-clang-ghc881',
                       ],
                ),
                BldSet('regular', 'submodules',
                       goods=['master.submodules-gnucc-ghc844',
                              'master.submodules-gnucc-ghc865',
                              'master.submodules-gnucc-ghc881',
                       ],
                       fails=['master.submodules-clang-ghc844',
                              # 'master.submodules-clang-ghc865',
                              # 'master.submodules-clang-ghc881',
                       ],
                ),
            )),
        updated=[]) in reps
