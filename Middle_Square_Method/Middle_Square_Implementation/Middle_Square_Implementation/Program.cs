using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Middle_Square_Implementation
{
    class Program
    {
        static void Main(string[] args)
        {
            //The Middle Square Method works by multiplying a number by itself then selecting n digits from the result to be the generated value
            //This value is then multiplied by itself to continue the algorithm
            //Values generated are between 1 and 100

            //Set up variables before generation
            //A shorter seed value was required as multiplication of 30102000 led to an overflow error
            const int seed = 301020;
            int value = 0;
            int valueLength = 0;
            int valueMiddle = 0;
            string valueString = "";
            List<int> ReturnValues = new List<int>();

            value = seed;
            //Also to keep results fair, all generators use the same number of iterations
            for (int i = 1; i <= 100; i++)
            {
                value = value * value;
                valueLength = value.ToString().Length;

                //Middle values cannot be extracted from even length squares
                //Padding using 0 to make the length odd is required for successful generation
                valueString = Convert.ToString(value);

                if (valueLength % 2 == 0)
                {
                    valueString = valueString.Insert(0, "0");
                    valueLength = valueString.Length;
                }

                //Using substrings, the middle 2 digits of the valueString variable can be obtained
                valueMiddle = Convert.ToInt32(valueString.Substring((valueLength / 2) - 1, 2));

                Console.WriteLine(valueMiddle);

                //All generated values are stored in the ReturnValues list
                //On inspection of the outputs given, the flaws of this algorithm become clear
                //Repetitions begin with this seed after 5 iterations
                ReturnValues.Add(valueMiddle);

                value = valueMiddle;
            }

            Console.ReadLine();
        }
    }
}
