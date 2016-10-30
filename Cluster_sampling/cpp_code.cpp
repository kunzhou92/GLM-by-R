#include<iostream>
#include<random>
#include<time.h>
#include<set>
#include<math.h>
#include<utility>
#include<fstream>
#include <ctime>

using namespace std;

class ClusterSample
{
private:
	int vertex[64][64];         // 0 black; 1 white
	bool edge[64][64][2];        // false off; true on
	int clustered[64][64];
	vector<int> label;
	double sweep;
	double beta;
	double size;

	void update_edge();
	void find_cluster(int i, int j, int _label);
	void flip_all();
	void flip_one(int _label);
	void clear();

public:
	ClusterSample(double _beta, int _type, int rand = 1);              // 0 constant; 1 checkboard
	void version1();
	void version2();
	double return_h();
	double return_sweep();
	void output();
	double return_size();
};
ClusterSample::ClusterSample(double _beta, int _type, int rand)
{
	beta = _beta;
	sweep = 0;
	srand((unsigned)time(NULL)*rand);
	size = 0;
	for (int i = 0; i < 64; i++)
		for (int j = 0; j < 64; j++)
			clustered[i][j] = 0;

	if (_type == 0)
	{
		for (int i = 0; i < 64; i++)
		{
			for (int j = 0; j < 64; j++)
			{
				vertex[i][j] = 0;
				clustered[i][j] = false;
			}
		}
	}
	else if (_type == 1)
	{
		for (int i = 0; i < 64; i++)
		{
			for (int j = 0; j < 64; j++)
			{
				vertex[i][j] = (i + j) % 2;
				clustered[i][j] = false;
			}
		}
	}
}
void ClusterSample::update_edge()
{
	for (int i = 0; i < 64; i++)
	{
		for (int j = 0; j < 64; j++)
		{
			if (vertex[i][j] != vertex[i][(j + 1) % 64])
				edge[i][j][0] = false;
			else
			{
				if ((rand()*1.0 / RAND_MAX) < exp(-beta))
					edge[i][j][0] = false;
				else
					edge[i][j][0] = true;
			}
			if (vertex[i][j] != vertex[(i + 1) % 64][j])
				edge[i][j][1] = false;
			else
			{
				if ((rand()*1.0 / RAND_MAX) < exp(-beta))
					edge[i][j][1] = false;
				else
					edge[i][j][1] = true;
			}
		}
	}
}
void ClusterSample::find_cluster(int i, int j, int _label)
{
	if (clustered[i][j]==0)
	{

		clustered[i][j] = _label;
	}
	if (clustered[(i + 63) % 64][j] == 0 && edge[(i + 63) % 64][j][1])  //up
		find_cluster((i + 63) % 64, j, _label);
	
	if (clustered[(i + 1) % 64][j] == 0 && edge[i][j][1]) // down
		find_cluster((i + 1) % 64, j, _label);
	
	if (clustered[i][(j + 63) % 64] == 0 && edge[i][(j + 63) % 64][0]) //left
		find_cluster(i, (j + 63) % 64, _label);
		
	if (clustered[i][(j + 1) % 64] == 0 && edge[i][j][0]) //right
		find_cluster(i, (j + 1) % 64, +_label);
}
void ClusterSample::flip_all()
{
	vector<double> p;
	for (int i = 0; i < label.size(); i++)
		p.push_back(rand()*1.0 / RAND_MAX < 0.5);
	for (int i = 0; i < 64; i++)
	{
		for (int j = 0; j < 64; j++)
		{
			if (p[clustered[i][j]-1] < 0.5)
				vertex[i][j] = 0;
			else
				vertex[i][j] = 1;
		}
		
	}
}
void ClusterSample::flip_one(int _label) 
{
	double p = rand()*1.0 / RAND_MAX < 0.5;
	if (p < 0.5)
	{
		for (int i = 0; i < 64; i++)
		{
			for (int j = 0; j < 64; j++)
			{
				if (clustered[i][j] == _label)
					vertex[i][j] = 0;
			}
		}
	}
	else
	{
		for (int i = 0; i < 64; i++)
		{
			for (int j = 0; j < 64; j++)
			{
				if (clustered[i][j] == _label)
					vertex[i][j] = 1;
			}
		}
	}


}
void ClusterSample::clear()
{
	for (int i = 0; i < 64; i++)
		for (int j = 0; j < 64; j++)
			clustered[i][j] = 0;
	label.clear();
}
void ClusterSample::version1()
{
	update_edge();
	int each = 0;
	for (int i = 0; i < 64; i++)
	{
		for (int j = 0; j < 64; j++)
		{
			if (clustered[i][j]==0)
			{
				each++;
				label.push_back(each);
				find_cluster(i, j, each);


			}
		}
	}
	
	size = 4096 * 1.0 / label.size();
	flip_all();
	sweep++;
	clear();
}
void ClusterSample::version2()
{
	int size = 0;
	int i = rand() % 64;
	int j = rand() % 64;
	update_edge();
	find_cluster(i, j, 1);
	flip_one(1);
	for (int i = 0; i < 64; i++)
		for (int j = 0; j < 64; j++)
			if (clustered[i][j] == 1)
				size++;
	sweep += size * 1.0 / 4096;
	clear();
}
double ClusterSample::return_h()
{
	double result = 0;
	for (int i = 0; i < 64; i++)
	{
		for (int j = 0; j < 64; j++)
		{
			if (vertex[i][j] != vertex[(i + 1) % 64][j])
				result++;
			if (vertex[i][j] != vertex[i][(j + 1) % 64])
				result++;
		}
	}
	return result / (8192);
}
double ClusterSample::return_sweep()
{
	return sweep;
}
void ClusterSample::output()
{
	for (int i = 0; i < 64; i++)
	{
		for (int j = 0; j < 64; j++)
		{
			cout << vertex[i][j] << " ";
		}
		cout << endl;
	}
	cout << endl << endl << endl;
}
double ClusterSample::return_size()
{
	return size;
}



int main()
{
	
	/*
	double epsilon = 0.001;
	double h = 0.1820;
	int count = 30;
	int interval = 1;
	ClusterSample ex = ClusterSample(.85, 1);
	ofstream file;
	file.open("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\file.txt");
	file.clear();
	while (abs(ex.return_h() - h) >= epsilon)
	{

		ex.version2();
		cout << ex.return_sweep() << " " << ex.return_h() << endl;
		if (interval < ex.return_sweep())
		{
			file << ex.return_sweep() << " " << ex.return_h() << endl;
			interval++;
		}

	}
	for (int i = 0; i < count; )
	{
		/
		ex.version2();
		cout << ex.return_sweep() << " " << ex.return_h() << endl;
		if (interval < ex.return_sweep())
		{
			file << ex.return_sweep() << " " << ex.return_h() << endl;
			interval++;
			i++;
		}
	}
		file.close();
	return 1;

	
	/*
	ClusterSample ex = ClusterSample(0.85, 1);
	ofstream file;
	file.open("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\file.txt");
	ex.version1();
	for (int i = 0; i < 60; i++)
	{
		ex.version1();
		file << ex.return_sweep() << " " << ex.return_size() << endl;
	}
	file.close();
	return 1;
	*/

	
	int count = 200;
	double epsilon = 0.0001;
	int interval = 0;
	ClusterSample ex1 = ClusterSample(1, 0);
	ClusterSample ex2 = ClusterSample(1, 1, 4);
	ofstream file, file2;
	file.open("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\file.txt");
	file2.open("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\file2.txt");
	for (; interval < count; )
	{
		ex1.version2();
		if (interval < ex1.return_sweep())
		{
			file << ex1.return_sweep() << " " << ex1.return_h() << endl;
			interval++;
		}
	}
	interval = 0;
	for (; interval < count; )
	{
		ex2.version2();
		if (interval < ex2.return_sweep())
		{
			file2 << ex2.return_sweep() << " " << ex2.return_h() << endl;
			interval++;
		}
	}





}
