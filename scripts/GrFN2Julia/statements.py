from abc import ABCMeta, abstractmethod


class Stmt:
    def __init__(self, out, code):
        self.output = out
        self.code = code
        self.target = out[:-2]

    def __str__(self):
        return "[S]"

    def __repr__(self):
        return self.__str__()

    def get_lines(self):
        return self.lines


class AssignStmt(Stmt):
    def __init__(self, out, code):
        super().__init__(out, code)
        self.relation = self.code

    def __str__(self):
        return f"<A>: ({self.output})"


class CondStmt(Stmt):
    def __init__(self, out, code):
        super().__init__(out, code)
        self.cond = self.code

    def __str__(self):
        return f"<C>: ({self.output})"


class DecisionStmt(Stmt):
    def __init__(self, out, code):
        super().__init__(out, code)

        (self.sat, _, self.cond, _, self.nonsat) = code.split(" ")

    def __str__(self):
        return f"<D>: ({self.output})"
