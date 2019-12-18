#include <iostream>
#include <sstream>		// std::ostringstream
#include <string>		// std::string
#include <stdexcept>	// std::out_of_range
#include <algorithm>	// std::max
#include <iomanip>		// std::setw
#include <utility>		// std::pair
#include <map>			// std::map
#include <cstring>		// std::strlen

template<typename _Ty>
class Matrix
{
private:
	_Ty *mData;
	unsigned mRows;
	unsigned mCols;
public:
	Matrix(unsigned rows, unsigned cols)
	{
		mRows = rows;
		mCols = cols;
		mData = new _Ty[rows * cols]{};
	}
	Matrix(const Matrix &other)
		: mData{ new _Ty[other.mRows * other.mCols] },
		mRows{ other.mRows }, mCols{other.mCols}
	{
		for (unsigned i{}; i < (other.mRows * other.mCols); ++i)
		{
			mData[i] = other.mData[i];
		}
	}
	template<typename _Ty2>
	Matrix(const Matrix<_Ty2> &other)
		: mData{ new _Ty[other.sizeRows() * other.sizeCols()] },
		mRows{ other.sizeRows() }, mCols{ other.sizeCols() }
	{
		for (unsigned i{}; i < other.sizeRows(); ++i)
		{
			for (unsigned j{}; j < other.sizeCols(); ++j)
			{
				this->get(i, j) = other.get(i, j);
			}
		}
	}
	Matrix& operator=(const Matrix &rhs)
	{
		if (this != &rhs)
		{
			delete[] mData;
			mData = new _Ty[rhs.mRows * rhs.mCols];
			mRows = rhs.mRows;
			mCols = rhs.mCols;
			for (unsigned i{}; i < (rhs.mRows * rhs.mCols); ++i)
			{
				mData[i] = rhs.mData[i];
			}
		}
		return *this;
	}
	template<typename _Ty2>
	Matrix& operator=(const Matrix<_Ty2> &rhs)
	{
		if (this != &rhs)
		{
			delete[] mData;
			mData = new _Ty[rhs.sizeRows() * rhs.sizeCols()];
			mRows = rhs.sizeRows();
			mCols = rhs.sizeCols();
			for (unsigned i{}; i < rhs.sizeRows(); ++i)
			{
				for (unsigned j{}; j < rhs.sizeCols(); ++j)
				{
					this->get(i, j) = rhs.get(i, j);
				}
			}
		}
		return *this;
	}
	unsigned sizeRows(void) const
	{
		return mRows;
	}
	unsigned sizeCols(void) const
	{
		return mCols;
	}
	_Ty& get(const unsigned &i, const unsigned &j)
	{
		if ((i > (mRows - 1)) || (j > (mCols - 1)))
		{
			throw std::out_of_range("index of matrix out of range");
		}
		return mData[(i * mRows) + j];
	}
	const _Ty& get(const unsigned &i, const unsigned &j) const
	{
		if ((i > (mRows - 1)) || (j > (mCols - 1)))
		{
			throw std::out_of_range("index of matrix out of range");
		}
		return mData[(i * mRows) + j];
	}
	~Matrix(void)
	{
		delete[] mData;
	}
};

namespace Dynamic
{
	enum class Offset{ NONE, LEFT, UP, DIAGONALY}; // смещение в таблице (дл€ восстановлени€ строки)

	inline
		std::pair<int, std::string> LCS(const char *_firstStr, const char *_secondStr,
			const unsigned &_firstLength, const unsigned &_secondLength, const int &w)
	{
		Matrix<int> L{ _firstLength + 1, _secondLength + 1 };
		Matrix<unsigned> kx{ _firstLength + 1, _secondLength + 1 };
		Matrix<unsigned> ky{ _firstLength + 1, _secondLength + 1 };
		Matrix<Offset> data{ _firstLength + 1, _secondLength + 1};
		for (unsigned i{}; i < (_firstLength + 1); i++)
		{
			for (unsigned j{}; j < (_secondLength + 1); j++)
			{
				if (!i || !j)
				{
					L.get(i, j) = 0;
					kx.get(i, j) = 0;
					ky.get(i, j) = 0;
					data.get(i, j) = Offset::NONE;
				}
				else if (_firstStr[i - 1] == _secondStr[j - 1])
				{
					const int Score = L.get(i - 1, j - 1) + 1 + w
						* (((kx.get(i - 1, j - 1) == (i - 1)) ? 0 : 1)
							- ((ky.get(i - 1, j - 1) == (j - 1)) ? 0 : 1));
					const int max = std::max({ Score, L.get(i - 1, j), L.get(i, j - 1) });
					if (max == Score)
					{
						kx.get(i, j) = i;
						ky.get(i, j) = j;
						L.get(i, j) = Score;
						data.get(i, j) = Offset::DIAGONALY;
					}
					else if (max == L.get(i - 1, j))
					{
						kx.get(i, j) = kx.get(i - 1, j);
						ky.get(i, j) = ky.get(i - 1, j);
						L.get(i, j) = L.get(i - 1, j);
						data.get(i, j) = Offset::UP;
					}
					else
					{
						kx.get(i, j) = kx.get(i, j - 1);
						ky.get(i, j) = ky.get(i, j - 1);
						L.get(i, j) = L.get(i, j - 1);
						data.get(i, j) = Offset::LEFT;
					}
				}
				else
				{
					if (L.get(i - 1, j) > L.get(i, j - 1))
					{
						kx.get(i, j) = kx.get(i - 1, j);
						ky.get(i, j) = ky.get(i - 1, j);
						L.get(i, j) = L.get(i - 1, j);
						data.get(i, j) = Offset::UP;
					}
					else
					{
						kx.get(i, j) = kx.get(i, j - 1);
						ky.get(i, j) = ky.get(i, j - 1);
						L.get(i, j) = L.get(i, j - 1);
						data.get(i, j) = Offset::LEFT;
					}
				}

			}
		}
		std::ostringstream osstr{};
		unsigned i = _firstLength;
		unsigned j = _secondLength;
		while (i > 0 && j > 0)
		{
			if (data.get(i, j) == Offset::DIAGONALY)
			{
				osstr << _firstStr[i - 1];
				i--; j--;
			}
			else if (data.get(i, j) == Offset::LEFT)
			{
				j--;
			}
			else if (data.get(i, j) == Offset::UP)
			{
				i--;
			}
		}
		std::string strRes{ std::move(osstr.str()) };
		std::reverse(strRes.begin(), strRes.end());
		/*
		///================================== ¬ывод матриц =======================================
		auto print = [=](const Matrix<int> &_matrix, const unsigned &_spaceNum)
		{
			for (unsigned i{}; i < _matrix.sizeRows(); ++i)
			{
				for (unsigned j{}; j < _matrix.sizeCols(); ++j)
				{
					std::cout << std::setw(_spaceNum) << _matrix.get(i, j) << std::setw(0) << ' ';
				}
				std::cout << std::endl;
			}
		};
		std::cout << std::endl << "------------------[ L ]--------------------" << std::endl;
		print(L, 4);
		std::cout << "------------------[ kx ]-------------------" << std::endl;
		print(kx, 4);
		std::cout << "------------------[ ky ]-------------------" << std::endl;
		print(ky, 4);
		std::cout << "------------------[ Data ]--------------------" << std::endl;
		for (unsigned i{}; i < data.sizeRows(); ++i)
		{
			for (unsigned j{}; j < data.sizeCols(); ++j)
			{
				switch (data.get(i, j))
				{
				case Offset::NONE:
					std::cout << "0 ";
					break;
				case Offset::DIAGONALY:
					std::cout << "D ";
					break;
				case Offset::LEFT:
					std::cout << "L ";
					break;
				case Offset::UP:
					std::cout << "U ";
					break;
				}
			}
			std::cout << std::endl;
		}
		std::cout << "-------------------------------------------" << std::endl;
		///=======================================================================================
		//*/
		return{ L.get(_firstLength, _secondLength), strRes };
	}
}

template<typename _Ty, typename _Ty2, typename _Ty3>
struct Triad
{
	// ѕеременные структуры
	_Ty first;      // 1-€ переменна€. score
	_Ty2 second;    /// 2-€ переменна€. i
	_Ty3 third;     /// 3-€ переменна€. j
	// ќпределени€ типов
	typedef _Ty first_type;
	typedef _Ty2 second_type;
	typedef _Ty3 third_type;
};

namespace Recursion
{
	using triad = Triad<int, int, int>; // »спользуем 'triad' вместо того, чтобы каждый раз писать аргументы шаблона

	inline
	triad LCS(const char *_firstStr, const char *_secondStr,
		const int &_idx_i, const int &_idx_j, const int &w)
	{
		if (!_idx_i || !_idx_j)
		{
			return{ 0, -1 , -1 };
		}
		triad res[]
		{
			LCS(_firstStr, _secondStr, _idx_i - 1, _idx_j, w),
			LCS(_firstStr, _secondStr, _idx_i, _idx_j - 1, w)
		};
		if (_firstStr[_idx_i - 1] == _secondStr[_idx_j - 1])
		{
			const triad subTaskOnScore = LCS(_firstStr, _secondStr, _idx_i - 1, _idx_j - 1, w);
			const int Score = subTaskOnScore.first + 1 + w
				* (((subTaskOnScore.second == (_idx_i - 1)) ? 0 : 1)
					- ((subTaskOnScore.third == (_idx_j - 1)) ? 0 : 1));
			const int Max = std::max({ Score, res[0].first, res[1].first });
			if (Max == Score)
			{
				return{ Score, _idx_i, _idx_j };
			}
			else if (Max == res[0].first)
			{
				return res[0];
			}
			else
			{
				return res[1];
			}
		}
		else
		{
			return std::max(res[0], res[1],
				[=](const triad &lhs, const triad &rhs) -> bool
			{
				return (lhs.first < rhs.first);
			});
		}
	}
}

namespace Memoization
{
	using triad = Triad<int, int, int>;

	std::map<std::pair<int, int>, triad> globalMemoryScore;

	inline
	triad _lcs(const char *_firstStr, const char *_secondStr,
		const int &_idx_i, const int &_idx_j, const int &w)
	{
		const std::pair<int, int> Key{ _idx_i, _idx_j };
		if (globalMemoryScore.count(Key))
		{
			return globalMemoryScore[Key];
		}
		if (!_idx_i || !_idx_j)
		{
			return (globalMemoryScore[Key] = { 0, -1 , -1 });
		}
		triad res[]
		{
			_lcs(_firstStr, _secondStr, _idx_i - 1, _idx_j, w),
			_lcs(_firstStr, _secondStr, _idx_i, _idx_j - 1, w)
		};
		if (_firstStr[_idx_i - 1] == _secondStr[_idx_j - 1])
		{
			const triad subTaskOnScore = _lcs(_firstStr, _secondStr, _idx_i - 1, _idx_j - 1, w);
			const int Score = subTaskOnScore.first + 1 + w
				* (((subTaskOnScore.second == (_idx_i - 1)) ? 0 : 1)
					- ((subTaskOnScore.third == (_idx_j - 1)) ? 0 : 1));
			const int Max = std::max({ Score, res[0].first, res[1].first });
			if (Max == Score)
			{
				return (globalMemoryScore[Key] = { Score, _idx_i , _idx_j });
			}
			else if (Max == res[0].first)
			{
				return (globalMemoryScore[Key] = res[0]);
			}
			else
			{
				return (globalMemoryScore[Key] = res[1]);
			}
		}
		else
		{
			return (globalMemoryScore[Key] = std::max(res[0], res[1],
				[=](const triad &lhs, const triad &rhs) -> bool
			{
				return (lhs.first < rhs.first);
			}));
		}
	}

	inline
	triad LCS(const char *x, const char *y,
			const int &_idx_i, const int &j, const int &w)
	{
		globalMemoryScore.clear();
		return _lcs(x, y, _idx_i, j, w);
	}
}

int main()
{
	char firstStr[]{ "FABABCCCC" };
	char secondStr[]{ "FCBCBCACA" };

	std::cout << "Str #1: " << firstStr << std::endl;
	std::cout << "Str #2: " << secondStr << std::endl;

	const int w = 0;
	const unsigned fLen = std::strlen(firstStr);
	const unsigned sLen = std::strlen(secondStr);
	
	auto resDynamic = Dynamic::LCS(firstStr, secondStr, fLen, sLen, w);
	std::cout << "Dynamic: " << resDynamic.first << "\t" << resDynamic.second << std::endl;
	std::cout << "Recurcione: " << Recursion::LCS(firstStr, secondStr, fLen, sLen, w).first << std::endl;
	std::cout << "Memoization: " << Memoization::LCS(firstStr, secondStr, fLen, sLen, w).first << std::endl;
}