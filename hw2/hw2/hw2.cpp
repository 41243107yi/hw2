#include <iostream>
#include <string>
#include <exception>
#include <ctime>
using namespace std;

class Polynomial; // 前向宣告

class Term {
    friend Polynomial;
    friend ostream& operator<<(ostream& os, const Polynomial& p);
private:
    float coef; // 係數
    int exp;    // 指數
};

class Polynomial {
    friend ostream& operator<<(ostream& os, const Polynomial& p);
    friend istream& operator>>(istream& input, Polynomial& p);
private:
    Term* termArray; // 多項式中非零項的陣列
    int capacity;   // termArray 的大小
    int terms; // 非零項的數量
public:
    Polynomial(); // Construct the polynomial p(x) = 0.
    Polynomial Add(Polynomial poly); // Return the sum of the polynomial *this and poly.
    Polynomial Mult(Polynomial poly); // Return the product of the polynomials this and poly.
    float Eval(float f); // Evaluate the polynomial this at f and return the result.
    void NewTerm(const float newCoef, const int newExp); // 新增項目
};

Polynomial::Polynomial() : capacity(2), terms(0) {
    this->termArray = new Term[capacity];
}

Polynomial Polynomial::Add(Polynomial poly) {
    Polynomial res;
    int* loc;
    loc = new int[poly.terms + this->terms];
    float* data;
    data = new float[poly.terms + this->terms];
    int use_len = 0;
    for (int i = 0; i < this->terms; i++) {
        int t = -1;
        for (int j = 0; j < use_len; j++) {
            if (this->termArray[i].exp == loc[j]) {
                t = j;
                continue;
            }
        }
        if (t == -1) {
            loc[use_len] = this->termArray[i].exp;
            data[use_len++] = this->termArray[i].coef;
        }
        else
             data[t] += this->termArray[i].coef;
    }
      for (int i = 0; i < poly.terms; i++) {  // 把參數poly放入陣列
         int t = -1; // -1表示未找到
            for (int j = 0; j < use_len; j++) {  // 走訪重複判斷
               if (poly.termArray[i].exp == loc[j]) {
                       t = j;
                       continue;
            }           
        }
         if (t == -1) {
            loc[use_len] = poly.termArray[i].exp;
            data[use_len++] = poly.termArray[i].coef;
         }
        else 
          data[t] += poly.termArray[i].coef;
    }
      for (int i = 0; i < use_len; i++)  // 存入新的多項式類別
             res.NewTerm(data[i], loc[i]);
     return res;
}

Polynomial Polynomial::Mult(Polynomial poly) {
       Polynomial res;
       int* loc = new int[poly.terms * this->terms];  // 指數的陣列
       float* data = new float[poly.terms * this->terms];  // 係數的陣列
       int use_len = 0;  // 陣列使用長度
      for (int i = 0; i < this->terms; i++) {  // 走訪自己的Term陣列
          for (int j = 0; j < poly.terms; j++) {  // 走訪參數poly的Term陣列
             float t_coef = this->termArray[i].coef * poly.termArray[j].coef;  // 計算係數
                  int t_exp = this->termArray[i].exp + poly.termArray[j].exp;  // 計算指數
                     int t = -1;  // -1表示未找到
       for (int k = 0; k < use_len; k++) {  // 走訪重複判斷
            if (t_exp == loc[k]) {
                     t = k;
                     continue;
                }
            }
            if (t == -1) {
              loc[use_len] = t_exp;
              data[use_len++] = t_coef;
            }
            else
                data[t] += t_coef;
        }
    }
      for (int i = 0; i < use_len; i++) {
          res.NewTerm(data[i], loc[i]);
      }
            return res;
}
float Polynomial::Eval(float f) { // 多項式求值
    float res = 0.0f;
     for (int i = 0; i < this->terms; i++) { // 走訪terms
         float temp = this->termArray[i].coef;
            for (int j = 0; j < this->termArray[i].exp; j++) // f^n
                temp *= f;
                 res += temp;
     }
       return res; // 回傳最終計算值
   }
void Polynomial::NewTerm(const float newCoef, const int newExp) {
     if (this->terms == this->capacity) { // 空間不足時重新配置空間
           this->capacity *= 2;
            Term * temp = new Term[this->capacity];
            copy(this->termArray, this->termArray + terms, temp);
            delete[] this->termArray;
            this->termArray = temp;
    }
       this->termArray[this->terms].coef = newCoef;
       this->termArray[this->terms++].exp = newExp;
}
ostream& operator<<(ostream& output, const Polynomial& p) {
    for (int i = 0; i < p.terms; i++) {
        if (p.termArray[i].coef == 0) continue;// 判斷係數是否為 0，跳過該項
          if (i == 0) { // 處理第一項
            output << p.termArray[i].coef;
        }
        else {           
           if (p.termArray[i].coef > 0) {// 處理後續項：根據係數的正負，決定是否加 "+"
                output << " + " << p.termArray[i].coef;
            }
            else {
                output << " - " << -p.termArray[i].coef;
            }
        }
        if (p.termArray[i].exp != 0) {// 處理指數
            output << "x";
            if (p.termArray[i].exp != 1) {
                output << "^" << p.termArray[i].exp;
            }
        }
    }
    return output;
}


 istream & operator>>(istream & input, Polynomial & p) {
     float t_coef;
     int t_exp;
     char tmp;
     bool plus = true;
     while (1) {
         input >> t_coef;
         if (!plus) {
             t_coef *= -1;
             plus = true;
         }
         input.get(tmp);
         if (tmp == '\n') { // 遇到換行符，代表結束一項
               p.NewTerm(t_coef, 0); // 所有項都存儲完畢
               break; // 跳出迴圈
  }
         input.ignore(1); // 忽略指數前的符號
         input >> t_exp; // 讀入指數
         p.NewTerm(t_coef, t_exp);
         input.get(tmp);
         if (tmp == '\n') break ; // 若下一個項目為負，代表下一項係數為負
         if (tmp == '-')  plus = false; // 判斷是否多項式結尾
}
 return input;
}
 int main() {
     clock_t start, finish;
     cout << "輸入格式 ax^n2+bx^n1+cx^n0+d (若為常數，可省略x^0)\n";
     Polynomial p1, p2;
     cout << "p1: ";
     cin >> p1;
     cout << "p2: ";
     cin >> p2;
     cout << "p1 = " << p1 << endl;
     cout << "p2 = " << p2 << endl;
     cout << "---------------------\n";
     cout << "Polynomial的newTerm()\n";
     float t_coef = 0.0f;
     int t_exp = 0;
     cout << "請輸入新增項的係數: ";
     cin >> t_coef;
     cout << "請輸入新增項的指數: ";
     cin >> t_exp;
     cout << "新增前的多項式= " << p1 << endl;
     p1.NewTerm(t_coef, t_exp);
     cout << "新增後的多項式= " << p1 << endl;
     cout << "---------------------\n";
     cout << "Polynomial的Eval()\n";
     float f = 0.0f;
     cout << "請輸入x值: ";
     cin >> f;
     cout << "多項式: " << p1 << endl;
     cout << "結果= " << p1.Eval(f) << endl;
     cout << "---------------------\n";
     cout << "Polynomial的Add()\n";
      cout << "p1 = " << p1 << endl;
      cout << "p2 = " << p2 << endl;
     start = clock();
      cout << "(" << p1 << ") + (" << p2 << ") = " << p1.Add(p2) << endl;
     finish = clock();
     cout << "Add()需時: " << (double)(finish - start) /
         CLOCKS_PER_SEC << "s" << endl;
      cout << "---------------------\n";
      cout << "Polynomial的Mult():\n";
      cout << "p1 = " << p1 << endl;
     cout << "p2 = " << p2 << endl;
      start = clock();
     cout << "(" << p1 << ") * (" << p2 << ") = " << p1.Mult(p2) << endl;
      finish = clock();
     cout << "Mult()需時: " << (double)(finish - start) / CLOCKS_PER_SEC <<
         "s" << endl;
     cout << "---------------------\n";
     return 0;
 }