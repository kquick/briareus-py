import Briareus.Input.Parser as Parser

def get_input_descr_and_VCS_info(input_spec,
                                 cachedir=None,
                                 verbose=False):
    parser = Parser.BISParser(verbose=verbose)
    input_desc = parser.parse(input_spec)
    return input_desc
