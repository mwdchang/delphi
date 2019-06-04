from delphi.GrFN.networks import GroundedFunctionNetwork

# data_dir = "tests/data/GrFN/"
# sys.path.insert(0, "tests/data/program_analysis")

G = GroundedFunctionNetwork.from_fortran_file("ESUP.for")
A = G.to_agraph()
A.draw("ESUP_GrFN.pdf", prog="dot")
