import sys
import re
from ast import *
from lists import *




#-------------------------------------------------------------
# global parameters
#-------------------------------------------------------------

DEBUG = False
# sys.setrecursionlimit(10000)


MOVE_RATIO     = 0.2
MOVE_SIZE      = 10
MOVE_ROUND     = 5

FRAME_DEPTH    = 1
FRAME_SIZE     = 20

NAME_PENALTY   = 1
IF_PENALTY     = 1
ASSIGN_PENALTY = 1


#-------------------------------------------------------------
# utilities
#-------------------------------------------------------------

IS = isinstance


def debug(*args):
    if DEBUG:
        print args


def dot():
    sys.stdout.write('.')


def isAlpha(c):
    return (c == '_'
            or ('0' <= c <= '9')
            or ('a' <= c <= 'z')
            or ('A' <= c <= 'Z'))


def div(m, n):
    if n == 0:
        return m
    else:
        return m/float(n)


# for debugging
def ps(s):
    v = parse(s).body[0]
    if IS(v, Expr):
        return v.value
    else:
        return v


def sz(s):
    return nodeSize(parse(s), True) - 1


def dp(s):
    return dump(parse(s))


def run(name, closure=True, debug=False):
    fullname1 = name + '1.py'
    fullname2 = name + '2.py'

    global DEBUG
    olddebug = DEBUG
    DEBUG = debug

    diff(fullname1, fullname2, closure)

    DEBUG = olddebug


def demo():
    run('demo')


def go():
    run('heavy')


def pf():
    import cProfile
    cProfile.run("run('heavy')", sort="cumulative")




#------------------------ file system support -----------------------

def pyFileName(filename):
    try:
        start = filename.rindex('/') + 1
    except ValueError:
        start = 0
    end = filename.rindex('.py')
    return filename[start:end]


## file system support
def parseFile(filename):
    f = open(filename, 'r');
    lines = f.read()
    ast = parse(lines)
    improveAST(ast, lines, filename, 'left')
    return ast




#-------------------------------------------------------------
#            tests and operations on AST nodes
#-------------------------------------------------------------

# get list of fields from a node
def nodeFields(node):
    ret = []
    for field in node._fields:
        if field <> 'ctx' and hasattr(node, field):
            ret.append(getattr(node, field))
    return ret



# get full source text where the node is from
def nodeSource(node):
    if hasattr(node, 'nodeSource'):
        return node.nodeSource
    else:
        return None



# utility for getting exact source code part of the node
def src(node):
    return node.nodeSource[node.nodeStart : node.nodeEnd]



def nodeStart(node):
    if (hasattr(node, 'nodeStart')):
        return node.nodeStart
    else:
        return 0



def nodeEnd(node):
    return node.nodeEnd



def isAtom(x):
    return type(x) in [int, str, bool, float]



def isDef(node):
    return IS(node, FunctionDef) or IS(node, ClassDef)



# whether a node is a "frame" which can contain others and be
# labeled
def isFrame(node):
    return type(node) in [ClassDef, FunctionDef, Import, ImportFrom]



def isEmptyContainer(node):

    if IS(node, List) and node.elts == []:
        return True
    if IS(node, Tuple) and node.elts == []:
        return True
    if IS(node, Dict) and node.keys == []:
        return True

    return False


def sameDef(node1, node2):
    if IS(node1, FunctionDef) and IS(node2, FunctionDef):
        return node1.name == node2.name
    elif IS(node1, ClassDef) and IS(node2, ClassDef):
        return node1.name == node2.name
    else:
        return False


def differentDef(node1, node2):
    if isDef(node1) and isDef(node2):
        return node1.name <> node2.name
    return False


# decide whether it is reasonable to consider two nodes to be
# moves of each other
def canMove(node1, node2, c):
    return (sameDef(node1, node2) or
            c <= (nodeSize(node1) + nodeSize(node2)) * MOVE_RATIO)


# whether the node is considered deleted or inserted because
# the other party matches a substructure of it.
def nodeFramed(node, changes):
    for c in changes:
        if (c.isFrame and (node == c.orig or node == c.cur)):
            return True
    return False



# helper for turning nested if statements into sequences,
# otherwise we will be trapped in the nested structure and find
# too many differences
def serializeIf(node):
    if IS(node, If):
        if not hasattr(node, 'nodeEnd'):
            print "has no end:", node

        newif = If(node.test, node.body, [])
        newif.lineno = node.lineno
        newif.col_offset = node.col_offset
        newif.nodeStart = node.nodeStart
        newif.nodeEnd = node.nodeEnd
        newif.nodeSource = node.nodeSource
        newif.fileName = node.fileName
        return [newif] + serializeIf(node.orelse)
    elif IS(node, list):
        ret = []
        for n in node:
            ret += serializeIf(n)
        return ret
    else:
        return [node]


def nodeName(node):
    if IS(node, Name):
        return node.id
    elif IS(node, FunctionDef) or IS(node, ClassDef):
        return node.name
    else:
        return None


def attr2str(node):
    if IS(node, Attribute):
        vName = attr2str(node.value)
        if vName <> None:
            return vName + "." + node.attr
        else:
            return None
    elif IS(node, Name):
        return node.id
    else:
        return None


### utility for counting size of terms
def nodeSize(node, test=False):

    if not test and hasattr(node, 'nodeSize'):
        ret = node.nodeSize

    elif IS(node, list):
        ret = sum(map(lambda x: nodeSize(x, test), node))

    elif isAtom(node):
        ret = 1

    elif IS(node, Name):
        ret = 1

    elif IS(node, Num):
        ret = 1

    elif IS(node, Str):
        ret = 1

    elif IS(node, Expr):
        ret = nodeSize(node.value, test)

    elif IS(node, AST):
        ret = 1 + sum(map(lambda x: nodeSize(x, test), nodeFields(node)))

    else:
        ret = 0

    if test:
        print "node:", node, "size=", ret

    if IS(node, AST):
        node.nodeSize = ret

    return ret




#------------------------------- types ------------------------------
# global storage of running stats
class Stat:
    def __init__(self):
        pass

stat = Stat()



# The difference between nodes are stored as a Change structure.
class Change:
    def __init__(self, orig, cur, cost, isFrame=False):
        self.orig = orig
        self.cur = cur
        if orig == None:
            self.cost = nodeSize(cur)
        elif cur == None:
            self.cost = nodeSize(orig)
        elif cost == 'all':
            self.cost = nodeSize(orig) + nodeSize(cur)
        else:
            self.cost = cost
        self.isFrame = isFrame
    def __repr__(self):
        fr = "F" if self.isFrame else "-"
        def hole(x):
            if x == None:
                return "[]"
            else:
                return x
        return ("(C:" + str(hole(self.orig)) + ":" + str(hole(self.cur))
                + ":" + str(self.cost) + ":" + str(self.similarity())
                + ":" + fr + ")")
    def similarity(self):
        total = nodeSize(self.orig) + nodeSize(self.cur)
        return 1 - div(self.cost, total)



# Three major kinds of changes:
# * modification
# * deletion
# *insertion
def modifyNode(node1, node2, cost):
    return loner(Change(node1, node2, cost))

def delNode(node):
    return loner(Change(node, None, nodeSize(node)))

def insNode(node):
    return loner(Change(None, node, nodeSize(node)))



# general cache table for acceleration
class Cache:
    def __init__(self):
        self.table = {}
    def __repr__(self):
        return "Cache:" + str(self.table)
    def __len__(self):
        return len(self.table)
    def put(self, key, value):
        self.table[key] = value
    def get(self, key):
        if self.table.has_key(key):
            return self.table[key]
        else:
            return None



# 2-D array table for memoization of dynamic programming
def createTable(x, y):
    table = []
    for i in range(x+1):
        table.append([None] * (y+1))
    return table

def tableLookup(t, x, y):
    return t[x][y]

def tablePut(t, x, y, v):
    t[x][y] = v





#-------------------------------------------------------------
#                  string distance function
#-------------------------------------------------------------

### diff cache for AST nodes
strDistCache = Cache()
def clearStrDistCache():
    global strDistCache
    strDistCache = Cache()


### string distance function
def strDist(s1, s2):
    cached = strDistCache.get((s1, s2))
    if cached <> None:
        return cached

    if len(s1) > 100 or len(s2) > 100:
        if s1 <> s2:
            return 2.0
        else:
            return 0

    table = createTable(len(s1), len(s2))
    d = dist1(table, s1, s2)
    ret = div(2*d, len(s1) + len(s2))

    strDistCache.put((s1, s2), ret)
    return ret


# the main dynamic programming part
# similar to the structure of diffList
def dist1(table, s1, s2):
    def memo(v):
        tablePut(table, len(s1), len(s2), v)
        return v

    cached = tableLookup(table, len(s1), len(s2))
    if (cached <> None):
        return cached

    if s1 == '':
        return memo(len(s2))
    elif s2 == '':
        return memo(len(s1))
    else:
        if s1[0] == s2[0]:
            d0 = 0
        elif s1[0].lower() == s2[0].lower():
            d0 = 1
        else:
            d0 = 2

        d0 = d0 + dist1(table, s1[1:], s2[1:])
        d1 = 1 + dist1(table, s1[1:], s2)
        d2 = 1 + dist1(table, s1, s2[1:])
        return memo(min(d0, d1, d2))




#-------------------------------------------------------------
#                        diff of nodes
#-------------------------------------------------------------

stat.diffCount = 0
def diffNode(node1, node2, env1, env2, depth, move):

    # try substructural diff
    def trysub((changes, cost)):
        if not move:
            return (changes, cost)
        elif canMove(node1, node2, cost):
            return (changes, cost)
        else:
            mc1 = diffSubNode(node1, node2, env1, env2, depth, move)
            if mc1 <> None:
                return mc1
            else:
                return (changes, cost)

    if IS(node1, list) and not IS(node2, list):
        return diffNode(node1, [node2], env1, env2, depth, move)

    if not IS(node1, list) and IS(node2, list):
        return diffNode([node1], node2, env1, env2, depth, move)

    if (IS(node1, list) and IS(node2, list)):
        node1 = serializeIf(node1)
        node2 = serializeIf(node2)
        table = createTable(len(node1), len(node2))
        return diffList(table, node1, node2, env1, env2, 0, move)

    # statistics
    stat.diffCount += 1
    if stat.diffCount % 1000 == 0:
        dot()

    if node1 == node2:
        return (modifyNode(node1, node2, 0), 0)

    if IS(node1, Num) and IS(node2, Num):
        if node1.n == node2.n:
            return (modifyNode(node1, node2, 0), 0)
        else:
            return (modifyNode(node1, node2, 1), 1)

    if IS(node1, Str) and IS(node2, Str):
        cost = strDist(node1.s, node2.s)
        return (modifyNode(node1, node2, cost), cost)

    if (IS(node1, Name) and IS(node2, Name)):
        v1 = lookup(node1.id, env1)
        v2 = lookup(node2.id, env2)
        if v1 <> v2 or (v1 == None and v2 == None):
            cost = strDist(node1.id, node2.id)
            return (modifyNode(node1, node2, cost), cost)
        else:                           # same variable
            return (modifyNode(node1, node2, 0), 0)

    if (IS(node1, Attribute) and IS(node2, Name) or
        IS(node1, Name) and IS(node2, Attribute) or
        IS(node1, Attribute) and IS(node2, Attribute)):
        s1 = attr2str(node1)
        s2 = attr2str(node2)
        if s1 <> None and s2 <> None:
            cost = strDist(s1, s2)
            return (modifyNode(node1, node2, cost), cost)
        # else fall through for things like f(x).y vs x.y

    # if (IS(node1, ClassDef) and IS(node2, ClassDef)):
    #     (m1, c1) = diffNode(node1.bases, node2.bases, env1, env2, depth, move)
    #     (m2, c2) = diffNode(node1.body, node2.body, env1, env2, depth, move)
    #     (m3, c3) = diffNode(node1.decorator_list, node2.decorator_list,
    #                         env1, env2, depth, move)
    #     changes = append(m1, m2, m3)
    #     cost = c1 + c2 + c3 + strDist(node1.name, node2.name)
    #     return trysub((changes, cost))

    # if (IS(node1, FunctionDef) and IS(node2, FunctionDef)):
    #     return trysub(diffFunctionDef(node1, node2,
    #                                   env1, env2, depth, move))

    # if (IS(node1, Assign) and IS(node2, Assign)):
    #     (m1, c1) = diffNode(node1.targets, node2.targets,
    #                         env1, env2, depth, move)
    #     (m2, c2) = diffNode(node1.value, node2.value,
    #                         env1, env2, depth, move)
    #     return (append(m1, m2), c1 * ASSIGN_PENALTY + c2)

    # # flatten nested if nodes
    # if IS(node1, If) and IS(node2, If):
    #     seq1 = serializeIf(node1)
    #     seq2 = serializeIf(node2)
    #     if len(seq1) > 1 and len(seq2) > 1:
    #         return diffNode(seq1, seq2, env1, env2, depth, move)
    #     else:
    #         (m0, c0) = diffNode(node1.test, node2.test, env1, env2, depth, move)
    #         (m1, c1) = diffNode(node1.body, node2.body, env1, env2, depth, move)
    #         (m2, c2) = diffNode(node1.orelse, node2.orelse, env1, env2, depth, move)
    #         changes = append(m0, m1, m2)
    #         cost = c0 * IF_PENALTY + c1 + c2
    #         return trysub((changes, cost))

    if IS(node1, Module) and IS(node2, Module):
        return diffNode(node1.body, node2.body, env1, env2, depth, move)

    # other AST nodes
    if (IS(node1, AST) and IS(node2, AST) and
        type(node1) == type(node2)):

        fs1 = nodeFields(node1)
        fs2 = nodeFields(node2)
        changes, cost = nil, 0

        for i in xrange(len(fs1)):
            (m, c) = diffNode(fs1[i], fs2[i], env1, env2, depth, move)
            changes = append(m, changes)
            cost += c

        return trysub((changes, cost))

    if (type(node1) == type(node2) and
             isEmptyContainer(node1) and isEmptyContainer(node2)):
        return (modifyNode(node1, node2, 0), 0)

    # all unmatched types and unequal values
    return trysub((append(delNode(node1), insNode(node2)),
                   nodeSize(node1) + nodeSize(node2)))





###################### diff of a FunctionDef #####################

# separate out because it is too long

def diffFunctionDef(node1, node2, env1, env2, depth, move):

    # positionals
    len1 = len(node1.args.args)
    len2 = len(node2.args.args)

    if len1 < len2:
        minlen = len1
        rest = node2.args.args[minlen:]
    else:
        minlen = len2
        rest = node1.args.args[minlen:]

    ma = nil
    for i in xrange(minlen):
        a1 = node1.args.args[i]
        a2 = node2.args.args[i]
        if IS(a1, Name) and IS(a2, Name) and a1.id <> a2.id:
            env1 = ext(a1.id, a2, env1)
            env2 = ext(a2.id, a2, env2)
        (m1, c1) = diffNode(a1, a2, env1, env2, depth, move)
        ma = append(m1, ma)

    # handle rest of the positionals
    ca = 0
    if rest <> []:
        if len1 < len2:
            for arg in rest:
                ma = append(insNode(arg), ma)
                ca += nodeSize(arg)
        else:
            for arg in rest:
                ma = append(delNode(arg), ma)
                ca += nodeSize(arg)

    # vararg
    va1 = node1.varargName
    va2 = node2.varargName
    if va1 <> None and va2 <> None:
        if va1.id <> va2.id:
            env1 = ext(va1.id, va2, env1)
            env2 = ext(va2.id, va2, env2)
        cost = strDist(va1.id, va2.id)
        ma = append(modifyNode(va1, va2, cost), ma)
        ca += cost
    elif va1 <> None or va2 <> None:
        cost = nodeSize(va1) if va1 <> None else nodeSize(va2)
        ma = append(modifyNode(va1, va2, cost), ma)
        ca += cost

    # kwarg
    ka1 = node1.kwargName
    ka2 = node2.kwargName
    if ka1 <> None and ka2 <> None:
        if ka1.id <> ka2.id:
            env1 = ext(ka1.id, ka2, env1)
            env2 = ext(ka2.id, ka2, env2)
        cost = strDist(ka1.id, ka2.id)
        ma = append(modifyNode(ka1, ka2, cost), ma)
        ca += cost
    elif ka1 <> None or ka2 <> None:
        cost = nodeSize(ka1) if ka1 <> None else nodeSize(ka2)
        ma = append(modifyNode(ka1, ka2, cost), ma)
        ca += cost

    # defaults and body
    (md, cd) = diffNode(node1.args.defaults, node2.args.defaults,
                        env1, env2, depth, move)
    (mb, cb) = diffNode(node1.body, node2.body, env1, env2, depth, move)

    # sum up cost. penalize functions with different names.
    cost = ca + cd + cb + strDist(node1.name, node2.name)
    if node1.name <> node2.name:
        cost = cost * NAME_PENALTY

    return (append(ma, md, mb), cost)





########################## diff of a list ##########################

# diffList is the main part of dynamic programming

def diffList(table, ls1, ls2, env1, env2, depth, move):

    def memo(v):
        tablePut(table, len(ls1), len(ls2), v)
        return v

    def guess(table, ls1, ls2, env1, env2):
        (m0, c0) = diffNode(ls1[0], ls2[0], env1, env2, depth, move)
        (m1, c1) = diffList(table, ls1[1:], ls2[1:], env1, env2, depth, move)
        cost1 = c1 + c0

        if ((isFrame(ls1[0]) and
             isFrame(ls2[0]) and
             not nodeFramed(ls1[0], m0) and
             not nodeFramed(ls2[0], m0))):
            frameChange = modifyNode(ls1[0], ls2[0], c0)
        else:
            frameChange = nil

        # short cut 1 (func and classes with same names)
        if canMove(ls1[0], ls2[0], c0):
            return (append(frameChange, m0, m1), cost1)

        else:  # do more work
            (m2, c2) = diffList(table, ls1[1:], ls2, env1, env2, depth, move)
            (m3, c3) = diffList(table, ls1, ls2[1:], env1, env2, depth, move)
            cost2 = c2 + nodeSize(ls1[0])
            cost3 = c3 + nodeSize(ls2[0])

            if (not differentDef(ls1[0], ls2[0]) and
                cost1 <= cost2 and cost1 <= cost3):
                return (append(frameChange, m0, m1), cost1)
            elif (cost2 <= cost3):
                return (append(delNode(ls1[0]), m2), cost2)
            else:
                return (append(insNode(ls2[0]), m3), cost3)

    # cache look up
    cached = tableLookup(table, len(ls1), len(ls2))
    if (cached <> None):
        return cached

    if (ls1 == [] and ls2 == []):
        return memo((nil, 0))

    elif (ls1 <> [] and ls2 <> []):
        return memo(guess(table, ls1, ls2, env1, env2))

    elif ls1 == []:
        d = nil
        for n in ls2:
            d = append(insNode(n), d)
        return memo((d, nodeSize(ls2)))

    else: # ls2 == []:
        d = nil
        for n in ls1:
            d = append(delNode(n), d)
        return memo((d, nodeSize(ls1)))




###################### diff into a subnode #######################

# Subnode diff is only used in the moving phase. There is no
# need to compare the substructure of two nodes in the first
# run, because they will be reconsidered if we just consider
# them to be complete deletion and insertions.

def diffSubNode(node1, node2, env1, env2, depth, move):

    if (depth >= FRAME_DEPTH or
        nodeSize(node1) < FRAME_SIZE or
        nodeSize(node2) < FRAME_SIZE):
        return None

    if IS(node1, AST) and IS(node2, AST):

        if nodeSize(node1) == nodeSize(node2):
            return None

        if IS(node1, Expr):
            node1 = node1.value

        if IS(node2, Expr):
            node2 = node2.value

        if (nodeSize(node1) < nodeSize(node2)):
            for f in nodeFields(node2):
                (m0, c0) = diffNode(node1, f, env1, env2, depth+1, move)
                if canMove(node1, f, c0):
                    if not IS(f, list):
                        m1 = modifyNode(node1, f, c0)
                    else:
                        m1 = nil
                    framecost = nodeSize(node2) - nodeSize(node1)
                    m2 = loner(Change(None, node2, framecost, True))
                    return (append(m2, m1, m0), c0 + framecost)

        if (nodeSize(node1) > nodeSize(node2)):
            for f in nodeFields(node1):
                (m0, c0) = diffNode(f, node2, env1, env2, depth+1, move)
                if canMove(f, node2, c0):
                    framecost = nodeSize(node1) - nodeSize(node2)
                    if not IS(f, list):
                        m1 = modifyNode(f, node2, c0)
                    else:
                        m1 = nil
                    m2 = loner(Change(node1, None, framecost, True))
                    return (append(m2, m1, m0), c0 + framecost)

    return None





##########################################################################
##                          move detection
##########################################################################
def moveCandidate(node):
    return (isDef(node) or nodeSize(node) >= MOVE_SIZE)


stat.moveCount = 0
stat.moveSavings = 0
def getmoves(ds, round=0):

    dels = pylist(filterlist(lambda p: (p.cur == None and
                                        moveCandidate(p.orig) and
                                        not p.isFrame),
                             ds))
    adds = pylist(filterlist(lambda p: (p.orig == None and
                                        moveCandidate(p.cur) and
                                        not p.isFrame),
                             ds))

    # print "dels=", dels
    # print "adds=", adds

    matched = []
    newChanges, total = nil, 0

    print("\n[getmoves #%d] %d * %d = %d pairs of nodes to consider ..."
          % (round, len(dels), len(adds), len(dels) * len(adds)))

    for d0 in dels:
        for a0 in adds:
            (node1, node2) = (d0.orig, a0.cur)
            (changes, cost) = diffNode(node1, node2, nil, nil, 0, True)
            nterms = nodeSize(node1) + nodeSize(node2)

            if (canMove(node1, node2, cost) or
                nodeFramed(node1, changes) or
                nodeFramed(node2, changes)):

                matched.append(d0)
                matched.append(a0)
                adds.remove(a0)
                newChanges = append(changes, newChanges)
                total += cost

                if (not nodeFramed(node1, changes) and
                    not nodeFramed(node2, changes) and
                    isDef(node1) and isDef(node2)):
                    newChanges = append(modifyNode(node1, node2, cost),
                                        newChanges)

                stat.moveSavings += nterms
                stat.moveCount +=1
                if stat.moveCount % 1000 == 0:
                    dot()

                break

    print("\n\t%d matched pairs found with %d new changes."
          % (len(pylist(matched)), len(pylist(newChanges))))

    # print "matches=", matched
    # print "newChanges=", newChanges

    return (matched, newChanges, total)



# Get moves repeatedly because new moves may introduce new
# deletions and insertions.

def closure(res):
    (changes, cost) = res
    matched = None
    moveround = 1

    while moveround <= MOVE_ROUND and matched <> []:
        (matched, newChanges, c) = getmoves(changes, moveround)
        moveround += 1
        # print "matched:", matched
        # print "changes:", changes
        changes = filterlist(lambda c: c not in matched, changes)
        changes = append(newChanges, changes)
        savings = sum(map(lambda p: nodeSize(p.orig) + nodeSize(p.cur), matched))
        cost = cost + c - savings
    return (changes, cost)





#-------------------------------------------------------------
#                   improvements to the AST
#-------------------------------------------------------------

allNodes1 = set()
allNodes2 = set()

def improveNode(node, s, idxmap, filename, side):

    if IS(node, list):
        for n in node:
            improveNode(n, s, idxmap, filename, side)

    elif IS(node, AST):

        if side == 'left':
            allNodes1.add(node)
        else:
            allNodes2.add(node)

        findNodeStart(node, s, idxmap)
        findNodeEnd(node, s, idxmap)
        addMissingNames(node, s, idxmap)

        node.nodeSource = s
        node.fileName = filename

        for f in nodeFields(node):
            improveNode(f, s, idxmap, filename, side)



def improveAST(node, s, filename, side):
    idxmap = buildIndexMap(s)
    improveNode(node, s, idxmap, filename, side)




#-------------------------------------------------------------
#            finding start and end index of nodes
#-------------------------------------------------------------

def findNodeStart(node, s, idxmap):

    if hasattr(node, 'nodeStart'):
        return node.nodeStart

    elif IS(node, list):
        ret = findNodeStart(node[0], s, idxmap)

    elif IS(node, Module):
        ret = findNodeStart(node.body[0], s, idxmap)

    elif IS(node, BinOp):
        leftstart = findNodeStart(node.left, s, idxmap)
        if leftstart <> None:
            ret = leftstart
        else:
            ret = mapIdx(idxmap, node.lineno, node.col_offset)

    elif hasattr(node, 'lineno'):
        if node.col_offset >= 0:
            ret = mapIdx(idxmap, node.lineno, node.col_offset)
        else:                           # special case for """ strings
            i = mapIdx(idxmap, node.lineno, node.col_offset)
            while i > 0 and i+2 < len(s) and s[i:i+3] <> '"""':
                i -= 1
            ret = i
    else:
        ret = None

    if ret == None and hasattr(node, 'lineno'):
        raise TypeError("got None for node that has lineno", node)

    if IS(node, AST) and ret <> None:
        node.nodeStart = ret

    return ret




def findNodeEnd(node, s, idxmap):

    if hasattr(node, 'nodeEnd'):
        return node.nodeEnd

    elif IS(node, list):
        ret = findNodeEnd(node[-1], s, idxmap)

    elif IS(node, Module):
        ret = findNodeEnd(node.body[-1], s, idxmap)

    elif IS(node, Expr):
        ret = findNodeEnd(node.value, s, idxmap)

    elif IS(node, Str):
        i = findNodeStart(node, s, idxmap)
        if i+2 < len(s) and s[i:i+3] == '"""':
            q = '"""'
            i += 3
        elif s[i] == '"':
            q = '"'
            i += 1
        elif s[i] == "'":
            q = "'"
            i += 1
        else:
            print "illegal:", i, s[i]
        ret = endSeq(s, q, i)

    elif IS(node, Name):
        ret = findNodeStart(node, s, idxmap) + len(node.id)

    elif IS(node, Attribute):
        ret = endSeq(s, node.attr, findNodeEnd(node.value, s, idxmap))

    elif IS(node, FunctionDef):
        # addMissingNames(node, s, idxmap)
        # ret = findNodeEnd(node.nameName, s, idxmap)
        ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, Lambda):
        ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, ClassDef):
        # addMissingNames(node, s, idxmap)
        # ret = findNodeEnd(node.nameName, s, idxmap)
        ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, Call):
        ret = matchParen(s, '(', ')', findNodeEnd(node.func, s, idxmap))

    elif IS(node, Yield):
        ret = findNodeEnd(node.value, s, idxmap)

    elif IS(node, Return):
        if node.value <> None:
            ret = findNodeEnd(node.value, s, idxmap)
        else:
            ret = findNodeStart(node, s, idxmap) + len('return')

    elif IS(node, Print):
        ret = startSeq(s, '\n', findNodeStart(node, s, idxmap))

    elif (IS(node, For) or
          IS(node, While) or
          IS(node, If) or
          IS(node, IfExp)):
        if node.orelse <> []:
            ret = findNodeEnd(node.orelse, s, idxmap)
        else:
            ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, Assign) or IS(node, AugAssign):
        ret = findNodeEnd(node.value, s, idxmap)

    elif IS(node, BinOp):
        ret = findNodeEnd(node.right, s, idxmap)

    elif IS(node, BoolOp):
        ret = findNodeEnd(node.values[-1], s, idxmap)

    elif IS(node, Compare):
        ret = findNodeEnd(node.comparators[-1], s, idxmap)

    elif IS(node, UnaryOp):
        ret = findNodeEnd(node.operand, s, idxmap)

    elif IS(node, Num):
        ret = findNodeStart(node, s, idxmap) + len(str(node.n))

    elif IS(node, List):
        ret = matchParen(s, '[', ']', findNodeStart(node, s, idxmap));

    elif IS(node, Subscript):
        ret = matchParen(s, '[', ']', findNodeStart(node, s, idxmap));

    elif IS(node, Tuple):
        ret = findNodeEnd(node.elts[-1], s, idxmap)

    elif IS(node, Dict):
        ret = matchParen(s, '{', '}', findNodeStart(node, s, idxmap));

    elif IS(node, TryExcept):
        if node.orelse <> []:
            ret = findNodeEnd(node.orelse, s, idxmap)
        elif node.handlers <> []:
            ret = findNodeEnd(node.handlers, s, idxmap)
        else:
            ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, ExceptHandler):
        ret = findNodeEnd(node.body, s, idxmap)

    elif IS(node, Pass):
        ret = findNodeStart(node, s, idxmap) + len('pass')

    elif IS(node, Break):
        ret = findNodeStart(node, s, idxmap) + len('break')

    elif IS(node, Continue):
        ret = findNodeStart(node, s, idxmap) + len('continue')

    elif IS(node, Global):
        ret = startSeq(s, '\n', findNodeStart(node, s, idxmap))

    elif IS(node, Import):
        ret = findNodeStart(node, s, idxmap) + len('import')

    elif IS(node, ImportFrom):
        ret = findNodeStart(node, s, idxmap) + len('from')

    else:
        # print "[findNodeEnd] unrecognized node:", node, "type:", type(node)
        start = findNodeStart(node, s, idxmap)
        if start <> None:
            ret = start + 3
        else:
            ret = None

    if ret == None and hasattr(node, 'lineno'):
        raise TypeError("got None for node that has lineno", node)

    if IS(node, AST) and ret <> None:
        node.nodeEnd = ret

    return ret




#-------------------------------------------------------------
#                    adding missing Names
#-------------------------------------------------------------

def addMissingNames(node, s, idxmap):

    if hasattr(node, 'extraAttribute'):
        return

    if IS(node, list):
        for n in node:
            addMissingNames(n, s, idxmap)

    elif IS(node, ClassDef):
        start = findNodeStart(node, s, idxmap) + len('class')
        node.nameName = str2Name(s, start, idxmap)
        node._fields += ('nameName',)

    elif IS(node, FunctionDef):
        start = findNodeStart(node, s, idxmap) + len('def')
        node.nameName = str2Name(s, start, idxmap)
        node._fields += ('nameName',)

        if node.args.vararg <> None:
            if len(node.args.args) > 0:
                vstart = findNodeEnd(node.args.args[-1], s, idxmap)
            else:
                vstart = findNodeEnd(node.nameName, s, idxmap)
            vname = str2Name(s, vstart, idxmap)
            node.varargName = vname
        else:
            node.varargName = None
        node._fields += ('varargName',)

        if node.args.kwarg <> None:
            if len(node.args.args) > 0:
                kstart = findNodeEnd(node.args.args[-1], s, idxmap)
            else:
                kstart = findNodeEnd(node.varargName, s, idxmap)
            kname = str2Name(s, kstart, idxmap)
            node.kwargName = kname
        else:
            node.kwargName = None
        node._fields += ('kwargName',)

    elif IS(node, Attribute):
        start = findNodeEnd(node.value, s, idxmap)
        name = str2Name(s, start, idxmap)
        node.attrName = name
        node._fields = ('value', 'attrName')  # remove attr for node size accuracy

    elif IS(node, Compare):
        node.opsName = convertOps(node.ops, s,
                                  findNodeStart(node, s, idxmap), idxmap)
        node._fields += ('opsName',)

    elif (IS(node, BoolOp) or
          IS(node, BinOp) or
          IS(node, UnaryOp) or
          IS(node, AugAssign)):
        if hasattr(node, 'left'):
            start = findNodeEnd(node.left, s, idxmap)
        else:
            start = findNodeStart(node, s, idxmap)
        ops = convertOps([node.op], s, start, idxmap)
        node.opName = ops[0]
        node._fields += ('opName',)

    elif IS(node, Import):
        nameNames = []
        next = findNodeStart(node, s, idxmap) + len('import')
        name = str2Name(s, next, idxmap)
        while name <> None and next < len(s) and s[next] <> '\n':
            nameNames.append(name)
            next = name.nodeEnd
            name = str2Name(s, next, idxmap)
        node.nameNames = nameNames
        node._fields += ('nameNames',)

    node.extraAttribute = True



#-------------------------------------------------------------
#              utilities used by improve AST functions
#-------------------------------------------------------------

# find a sequence in a string s, returning the start point
def startSeq(s, pat, start):
    try:
        return s.index(pat, start)
    except ValueError:
        return len(s)



# find a sequence in a string s, returning the end point
def endSeq(s, pat, start):
    try:
        return s.index(pat, start) + len(pat)
    except ValueError:
        return len(s)



# find matching close paren from start
def matchParen(s, open, close, start):
    while s[start] <> open and start < len(s):
        start += 1
    if start >= len(s):
        return len(s)

    left = 1
    i = start + 1
    while left > 0 and i < len(s):
        if s[i] == open:
            left += 1
        elif s[i] == close:
            left -= 1
        i += 1
    return i



# build table for lineno <-> index oonversion
def buildIndexMap(s):
    line = 0
    col = 0
    idx = 0
    idxmap = [0]
    while idx < len(s):
        if s[idx] == '\n':
            idxmap.append(idx + 1)
            line += 1
        idx += 1
    return idxmap



# convert (line, col) to offset index
def mapIdx(idxmap, line, col):
    return idxmap[line-1] + col



# convert offset index into (line, col)
def mapLineCol(idxmap, idx):
    line = 0
    for start in idxmap:
        if idx < start:
            break
        line += 1
    col = idx - idxmap[line-1]
    return (line, col)



# convert string to Name
def str2Name(s, start, idxmap):
    i = start;
    while i < len(s) and not isAlpha(s[i]):
        i += 1
    startIdx = i
    ret = []
    while i < len(s) and isAlpha(s[i]):
        ret.append(s[i])
        i += 1
    endIdx = i
    id1 = ''.join(ret)

    if id1 == '':
        return None
    else:
        name = Name(id1, None)
        name.nodeStart = startIdx
        name.nodeEnd = endIdx
        name.lineno, name.col_offset = mapLineCol(idxmap, startIdx)
        return name



def convertOps(ops, s, start, idxmap):
    syms = map(lambda op: opsMap[type(op)], ops)
    i = start
    j = 0
    ret = []
    while i < len(s) and j < len(syms):
        oplen = len(syms[j])
        if s[i:i+oplen] == syms[j]:
            opName = Name(syms[j], None)
            opName.nodeStart = i
            opName.nodeEnd = i+oplen
            opName.lineno, opName.col_offset = mapLineCol(idxmap, i)
            ret.append(opName)
            j += 1
            i = opName.nodeEnd
        else:
            i += 1
    return ret


# lookup table for operators for convertOps
opsMap = {
    # compare:
    Eq     : '==',
    NotEq  : '<>',
    Lt     : '<',
    LtE    : '<=',
    Gt     : '>',
    GtE    : '>=',
    In     : 'in',
    NotIn  : 'not in',

    # BoolOp
    Or  : 'or',
    And : 'and',
    Not : 'not',

    # BinOp
    Add  : '+',
    Sub  : '-',
    Mult : '*',
    Div  : '/',
    Mod  : '%',

    # UnaryOp
    USub : '-',
    UAdd : '+',
}






#-------------------------------------------------------------
#                        HTML generation
#-------------------------------------------------------------


#-------------------- types and utilities ----------------------

class Tag:
    def __init__(self, tag, idx, start=-1):
        self.tag = tag
        self.idx = idx
        self.start = start
    def __repr__(self):
        return "tag:" + str(self.tag) + ":" + str(self.idx)



# escape for HTML
def escape(s):
    s = s.replace('"', '&quot;')
    s = s.replace("'", '&#39;')
    s = s.replace("<", '&lt;')
    s = s.replace(">", '&gt;')
    return s



uidCount = -1
uidHash = {}
def clearUID():
    global uidCount, uidHash
    uidCount = -1
    uidHash = {}


def uid(node):
    if uidHash.has_key(node):
        return uidHash[node]

    global uidCount
    uidCount += 1
    uidHash[node] = str(uidCount)
    return str(uidCount)



def lineId(lineno):
    return 'L' + str(lineno);


def qs(s):
    return "'" + s + "'"



#-------------------- main HTML generating function ------------------

def genHTML(text, changes, side):
    ltags = lineTags(text)
    ctags = changeTags(text, changes, side)
    ktags = keywordTags(side)
    body = applyTags(text, ltags + ctags + ktags, side)

    out = []
    out.append('<html>\n')
    out.append('<head>\n')
    out.append('<META http-equiv="Content-Type" content="text/html; charset=utf-8">\n')
    out.append('<LINK href="diff.css" rel="stylesheet" type="text/css">\n')
    out.append('<script type="text/javascript" src="nav.js"></script>\n')
    out.append('</head>\n')
    out.append('<body>\n')

    out.append('<pre>\n')
    out.append(body)
    out.append('</pre>\n')

    # out.append('</body>\n')
    # out.append('</html>\n')

    return ''.join(out)



# put the tags generated by changeTags into the text and create HTML
def applyTags(s, tags, side):
    tags = sorted(tags, key = lambda t: (t.idx, -t.start))
    curr = 0
    out = []
    for t in tags:
        while curr < t.idx and curr < len(s):
            out.append(escape(s[curr]))
            curr += 1
        out.append(t.tag)

    while curr < len(s):
        out.append(escape(s[curr]))
        curr += 1
    return ''.join(out)




#--------------------- tag generation functions ----------------------

def changeTags(s, changes, side):
    tags = []
    for r in changes:
        key = r.orig if side == 'left' else r.cur
        if hasattr(key, 'lineno'):
            start = nodeStart(key)
            if IS(key, FunctionDef):
                end = start + len('def')
            elif IS(key, ClassDef):
                end = start + len('class')
            else:
                end = nodeEnd(key)

            if r.orig <> None and r.cur <> None:
                # <a ...> for change and move
                tags.append(Tag(linkTagStart(r, side), start))
                tags.append(Tag("</a>", end, start))
            else:
                # <span ...> for deletion and insertion
                tags.append(Tag(spanStart(r), start))
                tags.append(Tag('</span>', end, start))

    return tags



def lineTags(s):
    out = []
    lineno = 1;
    curr = 0
    while curr < len(s):
        if curr == 0 or s[curr-1] == '\n':
            out.append(Tag('<div class="line" id="L' + str(lineno) + '">', curr))
            out.append(Tag('<span class="lineno">' + str(lineno) + ' </span>', curr))
        if s[curr] == '\n':
            out.append(Tag('</div>', curr))
            lineno += 1
        curr += 1
    out.append(Tag('</div>', curr))
    return out



def keywordTags(side):
    tags = []
    allNodes = allNodes1 if side == 'left' else allNodes2
    for node in allNodes:
        if type(node) in keywordMap:
            kw = keywordMap[type(node)]
            start = nodeStart(node)
            if src(node)[:len(kw)] == kw:
                startTag = (Tag('<span class="keyword">', start))
                tags.append(startTag)
                endTag = Tag('</span>', start + len(kw), start)
                tags.append(endTag)
    return tags


def spanStart(diff):
    if diff.cur == None:
        cls = "deletion"
    else:
        cls = "insertion"
    text = escape(describeChange(diff))
    return '<span class="' + cls + '" title="' + text + '">'



def linkTagStart(diff, side):
    if side == 'left':
        me, other = diff.orig, diff.cur
    else:
        me, other = diff.cur, diff.orig

    text = escape(describeChange(diff))
    if diff.cost > 0:
        cls = "change"
    else:
        cls = "move"

    return ('<a id="' + uid(me) + '" '
            + ' class="' + cls + '" '
            + ' title="' + text + '" '
            + 'onclick="highlight('
                          + qs(uid(me)) + ","
                          + qs(uid(other)) + ","
                          + qs(lineId(me.lineno)) + ","
                          + qs(lineId(other.lineno)) + ')">')


keywordMap = {
    FunctionDef : 'def',
    ClassDef    : 'class',
    For         : 'for',
    While       : 'while',
    If          : 'if',
    With        : 'with',
    Return      : 'return',
    Yield       : 'yield',
    Global      : 'global',
    Raise       : 'raise',
    Pass        : 'pass',
    TryExcept   : 'try',
    TryFinally  : 'try',
    }




# human readable description of node

def describeNode(node):

    def code(s):
        return "'" + s + "'"

    def short(node):
        if IS(node, Module):
            ret = "module"
        elif IS(node, Import):
            ret = "import statement"
        elif IS(node, Name):
            ret = code(node.id)
        elif IS(node, Attribute):
            ret = code(short(node.value) + "." + short(node.attrName))
        elif IS(node, FunctionDef):
            ret = "function " + code(node.name)
        elif IS(node, ClassDef):
            ret = "class " + code(node.name)
        elif IS(node, Call):
            ret = "call to " + code(short(node.func))
        elif IS(node, Assign):
            ret = "assignment"
        elif IS(node, If):
            ret = "if statement"
        elif IS(node, While):
            ret = "while loop"
        elif IS(node, For):
            ret = "for loop"
        elif IS(node, Yield):
            ret = "yield"
        elif IS(node, TryExcept) or IS(node, TryFinally):
            ret = "try statement"
        elif IS(node, Compare):
            ret = "comparison " + src(node)
        elif IS(node, Return):
            ret = "return " + short(node.value)
        elif IS(node, Print):
            ret = ("print " + short(node.dest) +
                   ", " if (node.dest!=None) else "" + printList(node.values))
        elif IS(node, Expr):
            ret = "expression " + short(node.value)
        elif IS(node, Num):
            ret = str(node.n)
        elif IS(node, Str):
            if len(node.s) > 20:
                ret = "string " + code(node.s[:20]) + "..."
            else:
                ret = "string " + code(node.s)
        elif IS(node, Tuple):
            ret = "tuple (" + src(node) + ")"
        elif IS(node, BinOp):
            ret = (short(node.left) + " " +
                   node.opName.id + " " + short(node.right))
        elif IS(node, BoolOp):
            ret = src(node)
        elif IS(node, UnaryOp):
            ret = node.opName.id + " " + short(node.operand)
        elif IS(node, Pass):
            ret = "pass"
        elif IS(node, list):
            ret = map(short, node)
        else:
            ret = str(type(node))
        return ret

    ret = short(node)
    if hasattr(node, 'lineno'):
        ret = re.sub(" *(line [0-9]+)", '', ret)
        return ret + " (line " + str(node.lineno) + ")"
    else:
        return ret




# describe a change in a human readable fashion
def describeChange(diff):

    ratio = diff.similarity()
    sim = str(ratio)

    if ratio == 1.0:
        sim = " (unchanged)"
    else:
        sim = " (similarity %.1f%%)" % (ratio * 100)

    if diff.isFrame:
        wrap = "wrap "
    else:
        wrap = ""

    if diff.cur == None:
        ret = wrap + describeNode(diff.orig) + " deleted"
    elif diff.orig == None:
        ret = wrap + describeNode(diff.cur) + " inserted"
    elif nodeName(diff.orig) <> nodeName(diff.cur):
        ret = (describeNode(diff.orig) +
               " renamed to " + describeNode(diff.cur) + sim)
    elif diff.cost == 0 and diff.orig.lineno <> diff.cur.lineno:
        ret = (describeNode(diff.orig) +
               " moved to " + describeNode(diff.cur) + sim)
    elif diff.cost == 0:
        ret = describeNode(diff.orig) + " unchanged"
    else:
        ret = (describeNode(diff.orig) +
               " changed to " + describeNode(diff.cur) + sim)

    return ret





#-------------------------------------------------------------
#                     main HTML based command
#-------------------------------------------------------------

def diff(file1, file2, move=True):

    import time
    print("\nJob started at %s, %s\n" % (time.ctime(), time.tzname[0]))
    startTime = time.time()
    checkpoint(startTime)

    cleanUp()

    # base files names
    baseName1 = pyFileName(file1)
    baseName2 = pyFileName(file2)

    # get AST of file1
    f1 = open(file1, 'r');
    lines1 = f1.read()
    f1.close()
    node1 = parse(lines1)
    improveAST(node1, lines1, file1, 'left')

    # get AST of file2
    f2 = open(file2, 'r');
    lines2 = f2.read()
    f2.close()
    node2 = parse(lines2)
    improveAST(node2, lines2, file2, 'right')


    print("[parse] finished in %s. Now start to diff." % sec2min(checkpoint()))

    # get the changes

    (changes, cost) = diffNode(node1, node2, nil, nil, 0, False)

    print ("\n[diff] processed %d nodes in %s."
           % (stat.diffCount, sec2min(checkpoint())))

    if move:
#        print "changes:", changes
        (changes, cost) = closure((changes, cost))

        print("\n[closure] finished in %s." % sec2min(checkpoint()))



    #---------------------- print final stats ---------------------
    size1 = nodeSize(node1)
    size2 = nodeSize(node2)
    total = size1 + size2

    report = ""
    report += ("\n--------------------- summary -----------------------") + "\n"
    report += ("- total changes (chars):  %d" % cost)                  + "\n"
    report += ("- total code size:        %d (left: %d  right: %d)"
               % (total, size1, size2))                                + "\n"
    report += ("- total moved pieces:     %d" % stat.moveCount)        + "\n"
    report += ("- percentage of change:   %.1f%%"
               % (div(cost, total) * 100))                             + "\n"
    report += ("-----------------------------------------------------")   + "\n"

    print report


    #---------------------- generation HTML ---------------------
    # write left file
    leftChanges = filterlist(lambda p: p.orig <> None, changes)
    html1 = genHTML(lines1, leftChanges, 'left')

    outname1 = baseName1 + '.html'
    outfile1 = open(outname1, 'w')
    outfile1.write(html1)
    outfile1.write('<div class="stats"><pre class="stats">')
    outfile1.write(report)
    outfile1.write('</pre></div>')
    outfile1.write('</body>\n')
    outfile1.write('</html>\n')
    outfile1.close()


    # write right file
    rightChanges = filterlist(lambda p: p.cur <> None, changes)
    html2 = genHTML(lines2, rightChanges, 'right')

    outname2 = baseName2 + '.html'
    outfile2 = open(outname2, 'w')
    outfile2.write(html2)
    outfile2.write('<div class="stats"><pre class="stats">')
    outfile2.write(report)
    outfile2.write('</pre></div>')
    outfile2.write('</body>\n')
    outfile2.write('</html>\n')
    outfile2.close()


    # write frame file
    framename = baseName1 + "-" + baseName2 + ".html"
    framefile = open(framename, 'w')
    framefile.write('<frameset cols="50%,50%">\n')
    framefile.write('<frame name="left" src="' + baseName1 + '.html">\n')
    framefile.write('<frame name="right" src="' + baseName2 + '.html">\n')
    framefile.write('</frameset>\n')
    framefile.close()

    dur = time.time() - startTime
    print("\n[summary] Job finished at %s, %s" %
          (time.ctime(), time.tzname[0]))
    print("\n\tTotal duration: %s" % sec2min(dur))




def cleanUp():
    clearStrDistCache()
    clearUID()

    global allNodes1, allNodes2
    allNodes1 = set()
    allNodes2 = set()

    stat.diffCount = 0
    stat.moveCount = 0
    stat.moveSavings = 0



def sec2min(s):
    if s < 60:
        return ("%.1f seconds" % s)
    else:
        return ("%.1f minutes" % div(s, 60))



lastCheckpoint = None
def checkpoint(init=None):
    import time
    global lastCheckpoint
    if init <> None:
        lastCheckpoint = init
        return None
    else:
        dur = time.time() - lastCheckpoint
        lastCheckpoint = time.time()
        return dur




#-------------------------------------------------------------
#                      text-based interfaces
#-------------------------------------------------------------

## text-based main command
def printDiff(file1, file2):
    (m, c) = diffFile(file1, file2)
    print "----------", file1, "<<<", c, ">>>", file2, "-----------"

    ms = pylist(m)
    ms = sorted(ms, key=lambda d: nodeStart(d.orig))
    print "\n-------------------- changes(", len(ms), ")---------------------- "
    for m0 in ms:
        print m0

    print "\n-------------------  end  ----------------------- "




def diffFile(file1, file2):
    node1 = parseFile(file1)
    node2 = parseFile(file2)
    return closure(diffNode(node1, node2, nil, nil, 0, False))




# printing support for debugging use
def iter_fields(node):
    """Iterate over all existing fields, excluding 'ctx'."""
    for field in node._fields:
        try:
            if field <> 'ctx':
                yield field, getattr(node, field)
        except AttributeError:
            pass


def dump(node, annotate_fields=True, include_attributes=False):
    def _format(node):
        if isinstance(node, AST):
            fields = [(a, _format(b)) for a, b in iter_fields(node)]
            rv = '%s(%s' % (node.__class__.__name__, ', '.join(
                ('%s=%s' % field for field in fields)
                if annotate_fields else
                (b for a, b in fields)
            ))
            if include_attributes and node._attributes:
                rv += fields and ', ' or ' '
                rv += ', '.join('%s=%s' % (a, _format(getattr(node, a)))
                                for a in node._attributes)
            return rv + ')'
        elif isinstance(node, list):
            return '[%s]' % ', '.join(_format(x) for x in node)
        return repr(node)
    if not isinstance(node, AST):
        raise TypeError('expected AST, got %r' % node.__class__.__name__)
    return _format(node)

def printList(ls):
    if (ls == None or ls == []):
        return ""
    elif (len(ls) == 1):
        return str(ls[0])
    else:
        return str(ls)




# for debugging use
def printAst(node):
    if (IS(node, Module)):
        ret = "module:" + str(node.body)
    elif (IS(node, Name)):
        ret = str(node.id)
    elif (IS(node, Attribute)):
        if hasattr(node, 'attrName'):
            ret = str(node.value) + "." + str(node.attrName)
        else:
            ret = str(node.value) + "." + str(node.attr)
    elif (IS(node, FunctionDef)):
        if hasattr(node, 'nameName'):
            ret = "fun:" + str(node.nameName)
        else:
            ret = "fun:" + str(node.name)
    elif (IS(node, ClassDef)):
        ret = "class:" + str(node.name)
    elif (IS(node, Call)):
        ret = "call:" + str(node.func) + ":(" + printList(node.args) + ")"
    elif (IS(node, Assign)):
        ret = "(" + printList(node.targets) + " <- " + printAst(node.value) + ")"
    elif (IS(node, If)):
        ret = "if " + str(node.test) + ":" + printList(node.body) + ":" + printList(node.orelse)
    elif (IS(node, Compare)):
        ret = str(node.left) + ":" + printList(node.ops) + ":" + printList(node.comparators)
    elif (IS(node, Return)):
        ret = "return " + repr(node.value)
    elif (IS(node, Print)):
        ret = "print(" + (str(node.dest) + ", " if (node.dest!=None) else "") + printList(node.values) + ")"
    elif (IS(node, Expr)):
        ret = "expr:" + str(node.value)
    elif (IS(node, Num)):
        ret = "num:" + str(node.n)
    elif (IS(node, Str)):
        ret = 'str:"' + str(node.s) + '"'
    elif (IS(node, BinOp)):
        ret = str(node.left) + " " + str(node.op) + " " + str(node.right)
    elif (IS(node, Add)):
        ret = '+'
    elif (IS(node, Mult)):
        ret = '*'
    elif IS(node, NotEq):
        ret = '<>'
    elif (IS(node, Eq)):
        ret = '=='
    elif (IS(node, Pass)):
        ret = "pass"
    elif IS(node,list):
        ret = printList(node)
    else:
        ret = str(type(node))

    if hasattr(node, 'lineno'):
        return re.sub("@[0-9]+", '', ret) + "@" + str(node.lineno)
    elif hasattr(node, 'nodeStart'):
        return re.sub("@[0-9]+", '', ret) + "%" + str(nodeStart(node))
    else:
        return ret


def installPrinter():
    import inspect, ast
    for name, obj in inspect.getmembers(ast):
        if (inspect.isclass(obj) and not (obj == AST)):
            obj.__repr__ = printAst

installPrinter()

# demo
# diff('demos/demo1.py', 'demos/demo2.py')
