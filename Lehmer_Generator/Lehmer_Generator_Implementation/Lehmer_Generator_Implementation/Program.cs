using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lehmer_Generator_Implementation
{
    class Program
    {

        static void Main(string[] args)
        {
            //Park and Miller's implementation of the Lehmer Generator or minimal standard generator consists of 4 algorithms
            //Two algorithms dedicated to an integer based generator
            //And another two dedicated to a real number based generator

            //Declares the value to be given
            List<double> ReturnValues = new List<double>();

            //Integer Version 1
            IntegerVer1(ref ReturnValues);
            for (int i = 0; i <= ReturnValues.Count()-1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }
            Console.ReadLine();
        }

        public static void IntegerVer1(ref List<double>Values)
        {
            //Integer Versions of the algorithm use an integer based seed instead of a double or float
            //All values are between 1 and -1

            //Empties the list before generation
            Values.Clear();

            //Declares the seed multiplier (A) and the modifier (M) as integers
            //For the algorithms used the original 1988 value of A has been replaced by Park and Miller's updated value to ensure greater accuracy
            const int A = 48271;
            const int M = 214783647;

            //The base seed used for all algorithms featured will remain the same
            //The specific base seed used for testing is the authors date of birth (30/10/2000) as it provided a value large enough for authentic generation
            int seed = 30102000;

            //100 Generations are stored within ReturnValues
            for (int i = 1; i <= 100; i++)
            {
                seed = (A * seed) % M;

                //Since an int seed is used, conversion must take place otherwise all decimals are truncated to 0 before storage
                Values.Add((Convert.ToDouble(seed) / M));
            }
        }
    }
}
