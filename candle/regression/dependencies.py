import re
import common

DIRECTIVE = re.compile(r'\b(needs|loads)\s*"([^"]+)"\s*;;')
COMMENT = re.compile(r"\(\*|\*\)")


class Node:
    def __init__(self, filepath):
        self.filepath = filepath
        self.children = {}

    def insert(self, filepaths):
        node = self
        for filepath in filepaths:
            if filepath not in node.children:
                node.children[filepath] = Node(filepath)
            node = node.children[filepath]

    @classmethod
    def from_paths(cls, root_path, paths):
        root = cls(root_path)
        for path in paths:
            root.insert(path)
        return root

    def print(self, indent=0):
        print("  " * indent + self.filepath)
        for child in self.children.values():
            child.print(indent + 1)

    def leaves(self):
        if not self.children:
            return [self]
        result = []
        for child in self.children.values():
            result.extend(child.leaves())
        return result

    def _dot_edges(self, lines, counter):
        my_id = counter[0]
        counter[0] += 1
        lines.append(f'    n{my_id} [label="{self.filepath}"];')
        for child in self.children.values():
            child_id = counter[0]
            child._dot_edges(lines, counter)
            lines.append(f"    n{child_id} -> n{my_id};")

    def to_dot(self, path, name="dependencies"):
        lines = [f"digraph {name} {{"]
        self._dot_edges(lines, [0])
        lines.append("}")
        with open(path, "w") as f:
            f.write("\n".join(lines))


def strip_comments(src):
    out, depth, last = [], 0, 0
    for m in COMMENT.finditer(src):
        if m.group() == "(*":
            if depth == 0:
                out.append(src[last : m.start()])
            depth += 1
        elif depth:  # ignore unmatched *)
            depth -= 1
            if depth == 0:
                last = m.end()
    if depth == 0:
        out.append(src[last:])
    return "".join(out)


def process_directive(directive, loaded):
    cmd, fname = directive

    if cmd == "needs" and fname in loaded:
        return []
    loaded.add(fname)

    raw_src = common.resolve(fname).read_text(encoding="utf-8", errors="replace")
    src = strip_comments(raw_src)
    result = [
        item for m in DIRECTIVE.findall(src) for item in process_directive(m, loaded)
    ]
    result.append(fname)

    return result


def file_loads(file):
    return process_directive(("loads", file), set())


def dependency_tree(root_name, leaf_names):
    load_sequences = list(map(file_loads, leaf_names))
    return Node.from_paths(root_name, load_sequences)
