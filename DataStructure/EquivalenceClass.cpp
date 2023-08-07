#include <iostream>
#include <cstdio>
/*
	Equivalence Class
	201612980 고정현
*/
using namespace std;

class Node {
public:
	int a;
	Node* n; //next node
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
	if (Z == 1) { Z = 0; curr = head;  }
		else { curr = curr->n;  }
	if (curr == NULL) return 0;
		else return curr->a;
	}

int V[1000];
List LK[1000];
int SK[1000];
int SP;

void Push(int x) { SK[SP] = x; SP++; }
int Pop() { SP--; return SK[SP]; }
bool IsEmpty() { return SP == 0; } //23:01

int main()
{
	int n, m, i, cur, x, y, nn;
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
			cur = i; V[cur] = 1; printf("%d   ", cur);
			while (1) {
				nn = LK[cur].Next();
				if (nn != 0) {
					if(V[nn]==0){
					Push(cur); cur = nn; V[cur] = 1; printf("%d   ", cur);
					
					}
				}
				else {
					if (IsEmpty()) { break; }
					else {
						cur = Pop();
					}
				}
			}
			printf("\n");
		}
	}



	/*
	for (i = 0; i < n; i++) {
		T = LK[i].First();
		while (T != NULL) {
			printf("%d    ", T->a);
			T = LK[i].Next();
		}
		printf("\n");

	}
	*/
	return 0;
	
}










/*
10 7 노드 10개 7쌍이 들어온다

2 3
3 6
7 2
5 4
3 3
9 1
3 1

*/