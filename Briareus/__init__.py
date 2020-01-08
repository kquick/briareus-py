# Briareus build configuration specification tool

def print_each(title, collection, marker=None):
    print('%%',title,':')
    for each in collection:
        if marker:
            print(marker, str(each))
        else:
            print(str(each))

def print_titled(title, thing):
    print('##',title,':')
    print(str(thing))
