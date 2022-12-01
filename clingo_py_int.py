from clorm import Predicate, ConstantField, IntegerField, RawField
from clorm.clingo import Control
from clorm import *
import pydot

user_inputs_list = []
textfile = open("user_inputs.lp", "w")
for element in user_inputs_list:
    textfile.write(element + "\n")
textfile.close()

params=["max_ab_lvl(4).","generate_q(mustPayCorpTax(john,cakefactory)).","overrides(b_2,b_1).", "max_graph_level(3)."]
textfile = open("params.lp", "w")
for element in params:
    textfile.write(element + "\n")
textfile.close()


class Const(Predicate):
    val=IntegerField

class Opp_const(Predicate):
    val = IntegerField

class Ask_user(Predicate):
    name = RawField


class DirectedEdge(Predicate):
    sgn = ConstantField
    edge1 = RawField
    edge2 = RawField


d = 0
ctrl = Control(unifier=[Const,Opp_const,Ask_user,DirectedEdge])
ctrl.load("asp_transpiler_output.lp")
ctrl.load("asp_fixed_code_2.lp")
ctrl.load("params.lp")
ctrl.load("user_inputs.lp")

from clorm import FactBase

consts=[Const(val=0)]
opp_consts=[Opp_const(val=1)]

instance1 = FactBase(consts)
instance2 = FactBase(consts +opp_consts)

ctrl.add_facts(instance1)
ctrl.ground([("base",[])])

solution=None
def on_model(model):
    global solution     # Note: use `nonlocal` keyword depending on scope
    solution = model.facts(atoms=True)

ctrl.solve(on_model=on_model)
if not solution:
    #print('DB not notifiable')
    print("Query not legally true")
    d = 1
    q1_set=set()
#print(solution)
if solution:
    query1=solution.query(Ask_user)
    q1_set=set(query1.all())



#REPEAT for opp constraint

ctrl = Control(unifier=[Const,Opp_const,Ask_user,DirectedEdge])
ctrl.load("asp_transpiler_output.lp")
ctrl.load("asp_fixed_code_2.lp")
ctrl.load("params.lp")
ctrl.load("user_inputs.lp")

#instance1 = FactBase(consts)
#instance2 = FactBase(consts+opp_consts)

ctrl.add_facts(instance2)
ctrl.ground([("base",[])])

solution=None
def on_model(model):
    global solution     # Note: use `nonlocal` keyword depending on scope
    solution = model.facts(atoms=True)

ctrl.solve(on_model=on_model)
if not solution:
    #print('DB not notifiable')
    print("Query is legally true")
    d = 2
    q2_set=set()
#print(solution)
if solution:
    query1=solution.query(Ask_user)
    q2_set=set(query1.all())







if d==1:
    ctrl = Control(unifier=[Const,Opp_const,Ask_user,DirectedEdge])

    ctrl.load("asp_transpiler_output.lp")
    ctrl.load("asp_fixed_code_2.lp")
    ctrl.load("params.lp")
    ctrl.load("user_inputs.lp")

    ctrl.add_facts(instance2)
    ctrl.ground([("base",[])])

    solution=None
    def on_model(model):
        global solution     # Note: use `nonlocal` keyword depending on scope
        solution = model.facts(atoms=True)

    ctrl.solve(on_model=on_model)
    if not solution:
        #print('DB not notifiable')
        print("Error")

    #print(solution)
    if solution:
        query1=solution.query(DirectedEdge)
        edge_set_1=set(query1.all())

elif d==2:
    ctrl = Control(unifier=[Const,Opp_const,Ask_user,DirectedEdge])
    ctrl.load("asp_transpiler_output.lp")
    ctrl.load("asp_fixed_code_2.lp")
    ctrl.load("params.lp")
    ctrl.load("user_inputs.lp")



    ctrl.add_facts(instance1)
    ctrl.ground([("base",[])])

    solution=None
    def on_model(model):
        global solution     # Note: use `nonlocal` keyword depending on scope
        solution = model.facts(atoms=True)

    ctrl.solve(on_model=on_model)
    if not solution:
        #print('DB not notifiable')
        print("Error")

    #print(solution)
    if solution:
        query1=solution.query(DirectedEdge)
        edge_set_1=set(query1.all())


else:
    edge_set_1=set()

question_list=list(q1_set.union(q2_set))

for elt in question_list:
    print(elt)

edge_list=list(edge_set_1)

my_edge_lst = []
for edge in edge_list:
    my_edge_lst.append([str(edge.sgn),str(edge.edge1), str(edge.edge2)])


def listunion (xs, ys):
    return (xs + [i for i in ys if i not in xs])
def nodes_of_list (xs):
    a_list = list (map (lambda v: v[1], xs))
    b_list = list (map (lambda v: v[2], xs))
    return (listunion(a_list, b_list))
def generate_graph(glist):
    nds = nodes_of_list(glist)
    graph = pydot.Dot("my_graph", graph_type="digraph", bgcolor="white")
    for n in nds:
        graph.add_node(pydot.Node(n, shape="box"))

    for triple in glist:
        if triple[0] == 'pos':
            graph.add_edge(pydot.Edge(triple[1], triple[2], color="blue"))
        else:
            graph.add_edge(pydot.Edge(triple[1], triple[2], color="red"))


    graph.write_pdf("output.pdf")

generate_graph(my_edge_lst)
