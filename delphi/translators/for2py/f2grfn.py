"""
This program is for replacement of autoTranslate bash script.
Instead of creating and using each file for next operation
like in autoTranslate bash scropt, it creates python object
and passes it to the next function. Thus, it works as calling
a multiple functions in a single program. This new f2grfn.py
does not invoke main functions in each program.

In simplicity, it's a single program that integrated the
functionalities of test_program_analysis.py and autoTranslate.

Example:
    This script can be executed as below:

        $ python f2grfn -f <fortran_file>

fortran_file: An original input file to a program that is
to be translated to GrFN.

Author: Terrence J. Lim
"""

import os
import sys
import ast
import argparse
import pickle
import delphi.paths
import ntpath as np
import subprocess as sp
import xml.etree.ElementTree as ET

from delphi.translators.for2py import (
    preprocessor,
    translate,
    get_comments,
    pyTranslate,
    genPGM,
    mod_index_generator,
    rectify,
)

OFP_JAR_FILES = [
    "antlr-3.3-complete.jar",
    "commons-cli-1.4.jar",
    "OpenFortranParser-0.8.4-3.jar",
    "OpenFortranParserXML-0.4.1.jar",
]
"""OFP_JAR_FILES is a list of JAR files used by the Open Fortran Parser (OFP).
"""


def generate_ofp_xml(preprocessed_fortran_file, ofp_file, tester_call):
    """ This function executes Java command to run open
    fortran parser to generate initial AST XML from
    the preprocessed fortran file.

    Args:
        preprocessed_fortran_file (str): A preprocessed fortran file name.
        ofp_file (str): A file name that the OFP XML will be written to.
        tester_call (bool): A boolean condition that will indicate whether the
            program was invoked standalone (False) or by tester scripts (True).

    Returns:
        str: OFP generate XML in a sequence of strings.
    """

    if not tester_call:
        print(
            f"+$java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter --verbosity 0 {preprocessed_fortran_file}"
        )

    # Excute Java command to generate XML
    # string from fortran file
    ofp_xml = sp.run(
        [
            "java",
            "fortran.ofp.FrontEnd",
            "--class",
            "fortran.ofp.XMLPrinter",
            "--verbosity",
            "0",
            preprocessed_fortran_file,
        ],
        stdout=sp.PIPE,
    ).stdout

    if tester_call == False:
        # Indent and construct XML for file output
        ast = ET.XML(ofp_xml)
        indent(ast)
        tree = ET.ElementTree(ast)

        try:
            with open(ofp_file, "w") as f:
                pass
        except IOError:
            assert False, f"Failed to write to {ofp_file}."

        tree.write(ofp_file)

    return ofp_xml


def generate_rectified_xml(ofp_xml: str, rectified_file, tester_call):
    """
        This function rectifies XMl that was generated by
        OFP. Then, it will generate an output file, but
        also returns rectified element tree object back
        to the caller.

        Args:
            ofp_xml (str): A string of XML that was generated by OFP.
            rectified_file (str): A file name that rectified XML 
            will be written to.
            tester_call (bool): A boolean condition that will indicate
            whether the program was invoked standalone (False) or
            by tester scripts (True).

        Returns:
            Element Tree (ET) Object: An object of generated rectified
            XML.
    """

    if not tester_call:
        print(
            "+Generating rectified XML: Func: <buildNewASTfromXMLString>, Script: <rectify.py>"
        )

    rectified_xml = rectify.buildNewASTfromXMLString(ofp_xml)

    if tester_call == False:
        rectified_tree = ET.ElementTree(rectified_xml)
        try:
            with open(rectified_file, "w") as f:
                pass
        except IOError:
            assert False, f"Failed to write to {rectified_file}."

        rectified_tree.write(rectified_file)

    return rectified_xml


def generate_outputDict(
    rectified_tree, preprocessed_fortran_file, pickle_file, tester_call
):
    """
        This function generates a dictionary of ast and
        generates a pickle file.

        Args:
            rectified_tree (:obj: 'ET'): An object of rectified XML.
            preprocessed_fortran_file (str): A file name of preprocessed
            fortran file
            pickel_file (str): A file name where binary pickle will be
            written to.
            tester_call (bool): A boolean condition that will indicate
            whether the program was invoked standalone (False) or
            by tester scripts (True).


        Returns:
            dict: A dictionary of XML generated by translate.py
    """


    outputDict = translate.xml_to_py(
        [rectified_tree], preprocessed_fortran_file
    )

    if tester_call == False:
        print("+Generating pickle file: Func: <xml_to_py>, Script: <translate.py>")
        try:
            with open(pickle_file, "wb") as f:
                pickle.dump(outputDict, f)
        except IOError:
            assert False, f"Failed to write to {pickle_file}."

    return outputDict


def generate_python_src(outputDict, python_file, output_file, tester_call):
    """
        This function generates python source file from
        generated python source list. This function will
        return this list back to the caller for GrFN
        generation.

        Args:
            outputDict (dict): A dictionary of XML generated
            by translate.py.
            python_file (str): A file name where translated python strings
            will be written to.
            output_file (str): A file name where list of output file names
            will be written to.

        Returns:
            str: A string of generated python code.
    """


    pySrc = pyTranslate.create_python_source_list(outputDict)

    if not tester_call:
        print(
            "+Generating python source file:\
                Func: <create_python_source_list>,\
                Script: <pyTranslate.py>"
        )

        try:
            f = open(python_file, "w")
        except IOError:
            assert False, f"Unable to write to {python_file}."

        outputList = []
        for item in pySrc:
            outputList.append(python_file)
            f.write(item[0])

        try:
            with open(output_file, "w") as f:
                for fileName in outputList:
                    f.write(fileName + " ")
        except IOError:
            assert False, f"Unable to write to {outFile}."

    return pySrc[0][0]


def generate_grfn(
    python_src, python_file, lambdas_file, json_file, mode_mapper_dict, tester_call
):
    """
        This function generates GrFN dictionary object and file.

        Args:
            python_src (str): A string of python code.
            python_file (str): A file name of generated python script.
            lambdas_file (str): A file name where lambdas will be
            written to.
            json_file (str): A file name where JSON will be wrriten to.
            mode_mapper_dict (dict): A mapper of file info (i.e. filename,
            module, and exports, etc).

        Returns:
            dict: A dictionary of generated GrFN.
    """

    if not tester_call:
        print(
            "+Generating GrFN files: Func: <create_pgm_dict>, Script: <genPGM.py>"
        )

    asts = [ast.parse(python_src)]
    grfn_dict = genPGM.create_pgm_dict(
        lambdas_file, asts, python_file, mode_mapper_dict, save_file=True
    )
    for identifier in grfn_dict["identifiers"]:
        del identifier["gensyms"]

    return grfn_dict


def parse_args():
    """
        This function is for a safe command line
        input. It should receive the fortran file
        name and returns it back to the caller.

        Returns:
            str: A file name of original fortran script. 
    """
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-f", "--file", nargs="+", help="An input fortran file"
    )

    args = parser.parse_args(sys.argv[1:])

    fortran_file = args.file[0]

    return fortran_file


def check_classpath():
    """
        check_classpath() checks whether the files in OFP_JAR_FILES can all
        be found in via the environment variable CLASSPATH.
    """
    not_found = []
    classpath = os.environ["CLASSPATH"].split(":")
    for jar_file in OFP_JAR_FILES:
        found = False
        for path in classpath:
            dir_path = os.path.dirname(path)
            if path.endswith(jar_file) or (
                path.endswith("*") and jar_file in os.listdir(dir_path)
            ):
                found = True
                break
        if not found:
            not_found.append(jar_file)

    if not_found != []:
        sys.stderr.write("ERROR: JAR files not found via CLASSPATH:\n")
        sys.stderr.write(f" {','.join(not_found)}\n")
        sys.exit(1)


def indent(elem, level=0):
    """
        This function indents each level of XML.
        Source: https://stackoverflow.com/questions/3095434/inserting-newlines
                -in-xml-file-generated-via-xml-etree-elementstree-in-python

        Args:
            elem (:obj: 'ET'): An element tree XML object.
            level (int): A root level of XML.
    """
    i = "\n" + level * "  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level + 1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i


def get_original_file(original_fortran_file_path):
    """
        This function splits and extracts only the basename
        from the full path, then returns the tuple of
        (original_fortran_file, base) to the caller.

        Args:
            original_fortran_file_path (str): A string of path to
            the original fortran file.

        Returns:
            str {
                'original_fortran_file': A fortran file name with extention.
                'base': A fortran file without extention.
            }
    """
    original_fortran_file = np.basename(original_fortran_file_path)
    base = os.path.splitext(original_fortran_file)[0]

    return (original_fortran_file, base)


def fortran_to_grfn(
    original_fortran=None,
    tester_call=False,
    network_test=False,
    temp_dir="./tmp",
):
    """
        This function invokes other appropriate functions
        to process and generate objects to translate fortran
        to python IR. This function will either be invoked by
        local main function or the outer tester functions,
        such as test_program_analysis.py or network.py.

        Args:
            original_fortran (str): A file name of original fortran script.
            tester_call (bool): A boolean condition that will indicate
            whether the program was invoked standalone (False) or
            by tester scripts (True).
            network_test (bool): A boolean condition that will indicate
            whether the script was invoked by network.py or not.
            temp_dir (str): A default temporary directory where output
            files will be stored.

        Returns:
            str {
                'python_src': A string of python code,
                'python_file': A file name of generated python script,
                'lambdas_file': A file name where lambdas will be,
                'json_file': A file name where JSON will be wrriten to,

            }
            dict: mode_mapper_dict, mapper of file info (i.e. filename,
            module, and exports, etc).
    """
    check_classpath()

    # If "tmp" directory does not exist already,
    # simply create one.
    if not os.path.isdir(temp_dir):
        os.mkdir(temp_dir)

    # If, for2py runs manually by the user, which receives
    # the path to the file via command line argument
    if tester_call == False:
        original_fortran_file_path = parse_args()
    # Else, for2py function gets invoked by the test
    # programs, it will be passed with an argument
    # of original fortran file path
    else:
        original_fortran_file_path = original_fortran

    (original_fortran_file, base) = get_original_file(
        original_fortran_file_path
    )

    # Output files
    preprocessed_fortran_file = temp_dir + "/" + base + "_preprocessed.f"
    ofp_file = temp_dir + "/" + base + ".xml"
    rectified_xml_file = temp_dir + "/" + "rectified_" + base + ".xml"
    pickle_file = temp_dir + "/" + base + "_pickle"
    python_file = temp_dir + "/" + base + ".py"
    output_file = temp_dir + "/" + base + "_outputList.txt"
    json_file = temp_dir + "/" + base + ".json"
    lambdas_file = temp_dir + "/" + base + "_lambdas.py"

    # Open and read original fortran file
    try:
        with open(original_fortran_file_path, "r") as f:
            inputLines = f.readlines()
    except IOError:
        assert False, "Fortran file: {original_fortran_file_path} Not Found"

    # Preprocess the read in fortran file
    if not tester_call:
        print(
            "+Generating preprocessed fortran file:\
                Func: <process>, Script: <preprocessor.py>"
        )
    try:
        with open(preprocessed_fortran_file, "w") as f:
            f.write(preprocessor.process(inputLines))
    except IOError:
        assert False, "Unable to write tofile: {preprocessed_fortran_file}"

    # Generate OFP XML from preprocessed fortran
    ofp_xml = generate_ofp_xml(
        preprocessed_fortran_file, ofp_file, tester_call
    )

    # Rectify and generate a new xml from OFP XML
    rectified_tree = generate_rectified_xml(
        ofp_xml, rectified_xml_file, tester_call
    )

    if network_test == False:
        # Generate separate list of modules file
        mode_mapper_tree = rectified_tree
        generator = mod_index_generator.moduleGenerator()
        mode_mapper_dict = generator.analyze(mode_mapper_tree)

    # Creates a pickle file
    outputDict = generate_outputDict(
        rectified_tree, preprocessed_fortran_file, pickle_file, tester_call
    )

    # Create a python source file
    python_src = generate_python_src(
        outputDict, python_file, output_file, tester_call
    )

    if tester_call == True:
        os.remove(preprocessed_fortran_file)

    if network_test == False:
        return (
            python_src,
            lambdas_file,
            json_file,
            python_file,
            mode_mapper_dict,
        )
    else:
        return (python_src, lambdas_file, json_file, base)


if __name__ == "__main__":
    (
        python_src,
        lambdas_file,
        json_file,
        python_file,
        mode_mapper_dict,
    ) = fortran_to_grfn()

    # Generate GrFN file
    grfn_dict = generate_grfn(
        python_src, python_file, lambdas_file, json_file, mode_mapper_dict, False
    )
