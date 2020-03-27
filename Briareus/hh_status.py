#! /usr/bin/env nix-shell
#! nix-shell -i "python3.7 -u" -p "python37.withPackages(pp: with pp; [  attrs ])"

import argparse
from Briareus.AnaRep.Prior import ( read_report_from )
from Briareus.AnaRep.TextSummary import text_summary
from Briareus.AnaRep.HTMLSummary import html_summary

def text_formatter(repdata, _builder_url):
    lines = len(repdata)
    print('Text status from a', lines, ' line report')
    print(text_summary(repdata))

def html_formatter(repdata, builder_url):
    lines = len(repdata)
    # print('HTML status from a', lines, ' line report')
    print(html_summary(repdata, builder_url))

def main():
    supported_formats = { 'text': text_formatter,
                          'html': html_formatter,
    }

    parser = argparse.ArgumentParser(
        description='Show status output from Briareus build report.',
        epilog=('This tool simply formats the specified build report from the Briareus'
                ' hh (hundred hander) tool; it does not generate build configurations,'
                ' obtain build results, or perform any other actions.'),
        prog='hh_status')
    parser.add_argument(
        '--format', '-f', default='text', choices=list(supported_formats.keys()),
        help='Type of status output to generate',
    )
    parser.add_argument(
        '--builder-url', '-U', default=None, dest="builder_url",
        help="URL of builder for links to detailed build results."
        "  If not specified, no links will be generated.")
    # TBD: should -U come from the inp_configs as well Should it
    # specify the type of builder (like hh.py)?
    parser.add_argument(
        'INPUT_REPORT', type=argparse.FileType('r'),
        help="Briareus report file used as input (use '-' to read from stdin).")
    args = parser.parse_args()

    repdata = read_report_from(args.INPUT_REPORT)
    supported_formats[args.format](repdata, args.builder_url)

if __name__ == "__main__":
    main()
