#include <iostream>
#include <string>
#include <exception>
#include <ctime>
using namespace std;

class Polynomial; // �e�V�ŧi

class Term {
    friend Polynomial;
    friend ostream& operator<<(ostream& os, const Polynomial& p);
private:
    float coef; // �Y��
    int exp;    // ����
};

class Polynomial {
    friend ostream& operator<<(ostream& os, const Polynomial& p);
    friend istream& operator>>(istream& input, Polynomial& p);
private:
    Term* termArray; // �h�������D�s�����}�C
    int capacity;   // termArray ���j�p
    int terms; // �D�s�����ƶq
public:
    Polynomial(); // Construct the polynomial p(x) = 0.
    Polynomial Add(Polynomial poly); // Return the sum of the polynomial *this and poly.
    Polynomial Mult(Polynomial poly); // Return the product of the polynomials this and poly.
    float Eval(float f); // Evaluate the polynomial this at f and return the result.
    void NewTerm(const float newCoef, const int newExp); // �s�W����
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
      for (int i = 0; i < poly.terms; i++) {  // ��Ѽ�poly��J�}�C
         int t = -1; // -1��ܥ����
            for (int j = 0; j < use_len; j++) {  // ���X���ƧP�_
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
      for (int i = 0; i < use_len; i++)  // �s�J�s���h�������O
             res.NewTerm(data[i], loc[i]);
     return res;
}

Polynomial Polynomial::Mult(Polynomial poly) {
       Polynomial res;
       int* loc = new int[poly.terms * this->terms];  // ���ƪ��}�C
       float* data = new float[poly.terms * this->terms];  // �Y�ƪ��}�C
       int use_len = 0;  // �}�C�ϥΪ���
      for (int i = 0; i < this->terms; i++) {  // ���X�ۤv��Term�}�C
          for (int j = 0; j < poly.terms; j++) {  // ���X�Ѽ�poly��Term�}�C
             float t_coef = this->termArray[i].coef * poly.termArray[j].coef;  // �p��Y��
                  int t_exp = this->termArray[i].exp + poly.termArray[j].exp;  // �p�����
                     int t = -1;  // -1��ܥ����
       for (int k = 0; k < use_len; k++) {  // ���X���ƧP�_
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
float Polynomial::Eval(float f) { // �h�����D��
    float res = 0.0f;
     for (int i = 0; i < this->terms; i++) { // ���Xterms
         float temp = this->termArray[i].coef;
            for (int j = 0; j < this->termArray[i].exp; j++) // f^n
                temp *= f;
                 res += temp;
     }
       return res; // �^�ǳ̲׭p���
   }
void Polynomial::NewTerm(const float newCoef, const int newExp) {
     if (this->terms == this->capacity) { // �Ŷ������ɭ��s�t�m�Ŷ�
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
        if (p.termArray[i].coef == 0) continue;// �P�_�Y�ƬO�_�� 0�A���L�Ӷ�
          if (i == 0) { // �B�z�Ĥ@��
            output << p.termArray[i].coef;
        }
        else {           
           if (p.termArray[i].coef > 0) {// �B�z���򶵡G�ھګY�ƪ����t�A�M�w�O�_�[ "+"
                output << " + " << p.termArray[i].coef;
            }
            else {
                output << " - " << -p.termArray[i].coef;
            }
        }
        if (p.termArray[i].exp != 0) {// �B�z����
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
         if (tmp == '\n') { // �J�촫��šA�N�����@��
               p.NewTerm(t_coef, 0); // �Ҧ������s�x����
               break; // ���X�j��
  }
         input.ignore(1); // �������ƫe���Ÿ�
         input >> t_exp; // Ū�J����
         p.NewTerm(t_coef, t_exp);
         input.get(tmp);
         if (tmp == '\n') break ; // �Y�U�@�Ӷ��ج��t�A�N��U�@���Y�Ƭ��t
         if (tmp == '-')  plus = false; // �P�_�O�_�h��������
}
 return input;
}
 int main() {
     clock_t start, finish;
     cout << "��J�榡 ax^n2+bx^n1+cx^n0+d (�Y���`�ơA�i�ٲ�x^0)\n";
     Polynomial p1, p2;
     cout << "p1: ";
     cin >> p1;
     cout << "p2: ";
     cin >> p2;
     cout << "p1 = " << p1 << endl;
     cout << "p2 = " << p2 << endl;
     cout << "---------------------\n";
     cout << "Polynomial��newTerm()\n";
     float t_coef = 0.0f;
     int t_exp = 0;
     cout << "�п�J�s�W�����Y��: ";
     cin >> t_coef;
     cout << "�п�J�s�W��������: ";
     cin >> t_exp;
     cout << "�s�W�e���h����= " << p1 << endl;
     p1.NewTerm(t_coef, t_exp);
     cout << "�s�W�᪺�h����= " << p1 << endl;
     cout << "---------------------\n";
     cout << "Polynomial��Eval()\n";
     float f = 0.0f;
     cout << "�п�Jx��: ";
     cin >> f;
     cout << "�h����: " << p1 << endl;
     cout << "���G= " << p1.Eval(f) << endl;
     cout << "---------------------\n";
     cout << "Polynomial��Add()\n";
      cout << "p1 = " << p1 << endl;
      cout << "p2 = " << p2 << endl;
     start = clock();
      cout << "(" << p1 << ") + (" << p2 << ") = " << p1.Add(p2) << endl;
     finish = clock();
     cout << "Add()�ݮ�: " << (double)(finish - start) /
         CLOCKS_PER_SEC << "s" << endl;
      cout << "---------------------\n";
      cout << "Polynomial��Mult():\n";
      cout << "p1 = " << p1 << endl;
     cout << "p2 = " << p2 << endl;
      start = clock();
     cout << "(" << p1 << ") * (" << p2 << ") = " << p1.Mult(p2) << endl;
      finish = clock();
     cout << "Mult()�ݮ�: " << (double)(finish - start) / CLOCKS_PER_SEC <<
         "s" << endl;
     cout << "---------------------\n";
     return 0;
 }