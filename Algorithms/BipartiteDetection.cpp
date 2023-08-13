/*
bipartite detection
201612980 °íÁ¤Çö
*/

#include <iostream>
#include <cstdio>


using namespace std;

class Node {
public:
	int a;
	Node* n; 
};

class List {
public:
	List();
	int Insert(int x);
	void Zero();
	int Next();
	int Z;
	Node* head;
	Node* curr;

};

List::List() { head = NULL; curr = head; }

int List::Insert(int x)
{
	Node* NN;
	NN = new Node;
	NN->a = x;
	NN->n = head;
	head = NN;
	return 1;
}

void List::Zero() {
	Z = 1; 
}

int List::Next()
{
	if (Z == 1) { Z = 0; curr = head; }
	else { curr = curr->n; }
	if (curr == NULL) return 0;
	else return curr->a;
}

int V[1000]; 
List LK[1000];
int SK[1000];
int SP;

void Push(int x) { SK[SP] = x; SP++; }
int Pop() { SP--; return SK[SP]; }
bool IsEmpty() { return SP == 0; } 


int SKC[1000];
int SPC;

void PushC(int x) { SKC[SPC] = x; SPC++; }
int PopC() { SPC--; return SKC[SPC]; }
bool IsEmptyC() { return SPC == 0; }




int main()
{
	int n, m, i, cur, x, y, nn, mycolor, nextcolor, changemycolor;
	int fail;
	Node* T;
	scanf("%d %d", &n, &m);
	for (i = 0; i < m; i++) {
		scanf("%d %d", &x, &y);
		LK[x].Insert(y);
		LK[y].Insert(x);
	}
	for (i = 1; i <= n; i++) LK[i].Zero();
	for (i = 1; i <= n; i++)
	{
		

		if (V[i] == 0) { 
			mycolor = 1; fail = 0; changemycolor = 1;
			cur = i; V[cur] = mycolor; printf("%d   ", cur);
			while (1) {
				if (changemycolor == 1) {
					changemycolor = 0; 
					V[cur] = mycolor;
					if (mycolor == 1) nextcolor = 2;
					else nextcolor = 1;
				}
				nn = LK[cur].Next();
				if (nn != 0) {
					if (V[nn] == 0) {
						Push(cur); PushC(mycolor); cur = nn; V[cur] = nextcolor;
						mycolor = nextcolor; changemycolor = 1;
						printf("%d   ", cur);
					}
					else {
						if (V[nn] != nextcolor) {
							fail = 1;
							printf("??? node %d color %d next node %d color %d\n", cur, V[cur], nn, V[nn]);
						}
					}
				}
				else {
					if (IsEmpty()) { break; }
					else {
						cur = Pop(); mycolor = PopC(); changemycolor = 1; printf("*** node %d\n", cur);
					}
				}
			}
			printf("\n");
		}
	}
	if (fail == 1) printf("no\n");
	else printf("Yes\n");


	
	return 0;

}
