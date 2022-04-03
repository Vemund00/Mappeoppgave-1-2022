from cgitb import reset
import blotto
import numpy as np

def player_strategy(n_battalions,n_fields):
    #defining the array:
    battalions=np.zeros(n_fields,dtype=int)
    
    #assigning 25 battalions to the first four battle fields:
    battalions[0:1]=28
    battalions[1:2]=28
    battalions[2:3]=28
    battalions[3:4]=12
    battalions[4:5]=0
    battalions[5:6]=4
    
    #asserting that all and no more than all battalions are used:
    battalions=battalions[np.random.rand(n_fields).argsort()]
    assert sum(battalions)==n_battalions
    return battalions


def computer_strategy(n_battalions,n_fields):
    battalions=np.zeros(n_fields,dtype=int)
    battalions[0:1]=29
    battalions[1:2]=29
    battalions[2:3]=29
    battalions[3:4]=13
    battalions[4:5]=0
    battalions[5:6]=0
    assert sum(battalions)==n_battalions
    return battalions

#blotto.run(6, 100, player_strategy, computer_strategy)

def call_battle(n_battalions,n_fields, player_strategy, computer_strategy):
    c_battlions=computer_strategy(n_battalions,n_fields)
    p_battlions=player_strategy(n_battalions,n_fields)

    diff=p_battlions-c_battlions
    points=sum(diff>0)-sum(diff<0)
 
    return int(points>0)-int(points<0)


def test_strategies(n_fields,n_battalions,player_strategy, computer_strategy):
    n_tests=100000
    r=0
    record=[]
    for i in range(n_tests):
        p=call_battle(n_battalions,n_fields,
            player_strategy, computer_strategy)
        record.append(p)
        r+=p
    return r/n_tests

r=test_strategies(6,100,player_strategy, computer_strategy)
print(r)

if r > 0 :
    print("I won!")

if r < 0 :
    print("I lost!")