#include<iostream>
#include<cstdio>
/*
Dij
201612980 고정현
*/
using namespace std;

class PQ {
public:
	PQ();
	void Insert(pair <int, int> x);
	pair <int, int> Delete();
	int isEmpty();
	void print(int C, int depth, int LR);
	int s;
	pair<int, int> Arr[1000];
};

PQ::PQ() { s = 0; }

int PQ::isEmpty()
{
	return s == 0;
}

void PQ::Insert(pair<int, int> x)
{
	int i, j;
	pair <int, int> temp;
	s++;
	Arr[s] = x;
	i = s;
	while (i > 1) {
		j = i / 2;
		if (Arr[j] > Arr[i]) {
			temp = Arr[j];
			Arr[j] = Arr[i];
			Arr[i] = temp;
			i = j;
		}
		else {
			break;
		}
	}

}

pair <int, int> PQ::Delete()
{
	int i, j, k, p;
	pair <int, int> rt, temp;
	rt = Arr[1];
	Arr[1] = Arr[s];
	s--;
	i = 1;
	while (1) {
		j = i * 2; k = i * 2 + 1;
		if (j > s)break;
		if (k > s) {
			if (Arr[i] > Arr[j]) {
				temp = Arr[i]; Arr[i] = Arr[j]; Arr[j] = temp;
				i = j;
			}
			else {
				break;
			}
		}
		else {
			if (Arr[i] > Arr[j] || Arr[i] > Arr[k]) {
				if (Arr[j] < Arr[k]) p = j;
				else p = k;
				temp = Arr[i]; Arr[i] = Arr[p]; Arr[p] = temp;
				i = p;
			}
			else {
				break;
			}
		}
	}

	return rt;
}
void PQ::print(int C, int depth, int LR)
{
	if (s == 0) {
		printf("---\n");
		return;
	}
	if (depth == 0) {
		C = 1;
	}
	if (LR == 1) {
		int i;
		for (i = 0; i < depth; i++) printf("           ");
	}
	printf("---%03d, %03d", Arr[C].first, Arr[C].second);
	if (C * 2 <= s) {
		print(C * 2, depth + 1, 0);
	}
	else printf("\n");
	if (C * 2 + 1 <= s) {
		print(C * 2 + 1, depth + 1, 1);
	}
	else;
	return;
}

pair <int, int> LK[100][100];
int S[100]; // 링크가 몇 개 있냐를 의미
int D[100]; //노드마다 shortest path의 길이를 의미
int R[100]; //Red 표시

int n, m;

int main()
{
	char c;
	int i;
	int p, q, r;
	pair<int, int> t, pkt;
	PQ Q;
	scanf("%d %d", &n, &m);
	for (i = 1; i < n; i++)
		S[i] = 0; // 일단 링크가 없다고 함
	for (i = 1; i <= m; i++){
		scanf("%d %d %d", &p, &q, &r); // p번노드에서 q번노드로 가는 r짜리 링크가 있다
		S[p]++;
		LK[p][S[p]].first = q;
		LK[p][S[p]].second = r;
	}

	for (i = 1; i <= n; i++) {
		D[i] = 10000; R[i] = 0;
	}
		
	pkt.first = 0;
	pkt.second = 1;
	Q.Insert(pkt);

	while (Q.isEmpty() == 0) {
		for (i = 1; i <= n; i++)
			printf("%5d ", D[i]);
		printf("\n");
		Q.print(1, 0, 0);
		scanf("%c", &c);
		t = Q.Delete(); // Q: weight, node 번호
		r = t.first;
		q = t.second;
		if (R[q] == 1) continue;
		else {
			R[q] = 1;
			D[q] = r;
			for (i = 1; i <= S[q]; i++) {
				pkt.first = D[q] + LK[q][i].second;
				pkt.second = LK[q][i].first;
				Q.Insert(pkt);
			}
		}
	}
	return 0;
}

/*
* 입력
* 
Node 6개e edge 9개
1번에서 2번으로 4(1 2 4)
1번에서 3번으로 2(1 3 2)
3 2 1
2 4 5
3 4 1
3 5 8
4 5 2
4 6 6
5 6 3

(ㅁAother Example 슬라이드와 완전히 같음)
답: 0 2 3 8 10 13
*/
