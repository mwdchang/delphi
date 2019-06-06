import sys
import json
import inspect
import importlib

from statements import AssignStmt, CondStmt, DecisionStmt


def main():
    filename = sys.argv[1]

    # Load the GrFN file
    grfn = json.load(open(f"{filename}.json", "r"))

    # Load the Lambdas file
    lambdas = importlib.import_module(f"{filename}_lambdas")

    # Translate the model
    julia_src = translate_model(grfn, lambdas)

    # Write out the Julia source
    with open(f"{filename}.jl", "w") as outfile:
        outfile.write("\n".join(julia_src))


def get_julia_statements(stmt_stack):
    stmt_list = list()
    while len(stmt_stack) > 0:
        stmt = stmt_stack.pop(0)
        if isinstance(stmt, AssignStmt):
            stmt_list.append(f"\t{stmt.target} = {stmt.relation}")
        elif isinstance(stmt, DecisionStmt):
            cond, sat, nonsat = None, None, None
            i = 0
            while cond is None or sat is None or nonsat is None:
                new_stmt = stmt_stack[i]
                if isinstance(new_stmt, CondStmt):
                    cond = new_stmt.cond
                elif isinstance(new_stmt, AssignStmt):
                    if new_stmt.target == stmt.target:
                        if stmt.sat == new_stmt.output:
                            sat = new_stmt.relation
                            stmt_stack.pop(i)
                            continue
                        else:
                            nonsat = new_stmt.target

                elif isinstance(new_stmt, DecisionStmt):
                    if new_stmt.target == stmt.target:
                        nonsat = stmt.nonsat[:-2]

                i += 1

            stmt_list.append(f"\t{stmt.target} = ifelse({cond}, {sat}, {nonsat})")

        elif isinstance(stmt, CondStmt):
            pass
        else:
            raise ValueError(f"Unrecognized Stmt type: {type(stmt)}")

    stmt_list.reverse()
    return stmt_list


def translate_container(container, functions, lambdas, stmts, tab_level=1):
    for idiom in container["body"]:
        if hasattr(idiom, "function"):              # Process new container
            new_container = functions[idiom["function"]]
            translate_container(
                new_container, functions, lambdas, stmts, tab_level=tab_level+1
            )
        elif "__loop_plate__" in idiom["name"]:     # Process loop plate
            new_container = functions[idiom["name"]]
            translate_container(
                new_container, functions, lambdas, stmts, tab_level=tab_level+1
            )
        else:                                       # Handle regular statement
            lambda_fn = getattr(lambdas, idiom["name"])
            new_stmt = translate_stmt(idiom, functions, lambda_fn, tab_level)
            stmts.insert(0, new_stmt)


def translate_model(spec, lambdas):
    name = spec["start"]
    functions = {obj["name"]: obj for obj in spec["functions"]}
    init_container = functions[name]

    stmt_stack = list()
    translate_container(init_container, functions, lambdas, stmt_stack)

    stmt_list = get_julia_statements(stmt_stack)
    model_inputs = [i["name"] for i in init_container["input"]]
    stmt_list.insert(0, f"function {name}({', '.join(model_inputs)})")
    stmt_list.append("end")
    return stmt_list


def translate_stmt(stmt, functions, func, tab_level):
    print(stmt["name"])
    out_var = f"{stmt['output']['variable']}_{stmt['output']['index']}"
    stmt_type = functions[stmt["name"]]["type"]
    lines = inspect.getsource(func)
    rhs = lines.split("return")[1].strip()
    rhs = rhs.replace("**", "^").replace("math.", "")
    if rhs.startswith("(") and rhs.endswith(")"):
        rhs = rhs[1:-1]

    if stmt_type == "assign":
        return AssignStmt(out_var, rhs)
    elif stmt_type == "condition":
        return CondStmt(out_var, rhs)
    elif stmt_type == "decision":
        return DecisionStmt(out_var, rhs)
    else:
        raise ValueError(f"Unidentified statement type: {stmt['type']}")


if __name__ == '__main__':
    main()
