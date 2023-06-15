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
            Console.WriteLine("Integer Version 1");
            for (int i = 0; i <= ReturnValues.Count()-1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            Console.WriteLine("Real Version 1");

            //Real Version 1
            RealVer1(ref ReturnValues);
            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            Console.WriteLine("Integer Version 2");

            //Integer Version 2
            IntegerVer2(ref ReturnValues);
            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            Console.WriteLine("Real Version 2");

            //Real Version 2
            RealVer2(ref ReturnValues);
            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
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

        public static void RealVer1(ref List<double>Values)
        {
            //Real Versions of the algorithm use a double type variables
            //All values are between 1 and 0

            //Empties the list before generation
            Values.Clear();

            //As before constant values A and M are created
            //However double is used in place of int
            const double A = 48271.0;
            const double M = 2147483647.0;

            //Double is also used for both the seed and temp values
            double seed = 30102000.0;
            double temp = 0.0;

            //100 Generations are stored within ReturnValues
            for (int i = 1; i <= 100; i++)
            {
                temp = A * seed;
                seed = temp - M * Math.Truncate(temp / M);
                Values.Add(seed / M);
            }
        }

        public static void IntegerVer2(ref List<double>Values)
        {
            //All values are between 10 and -10
            Values.Clear();

            const int A = 48271;
            const int M = 214783647;
            int seed = 30102000;

            //Q and R are combinations of A and M that are used in the seed and test modification process
            const int Q = M / A; //Q is the result of M div A
            const int R = M % A; //R is the result of M mod A

            int lo = 0;
            int hi = 0;
            int test = 0;

            for (int i = 1; i <= 100; i++)
            {
                //Q relates to the modification of the base seed into a high and low value
                hi = seed / Q;
                lo = seed % Q;

                //R relates to the creation of test, based on further modification of hi and lo
                test = A * lo - R * hi;

                //If statement used to ensure that test will never be a value that cannot be correctly represented with 32 bits
                if (test > 0)
                {
                    seed = test;
                }
                else
                {
                    seed = test + M;
                }

                //Again due to the nature of int variables, conversion must be made to double to correctly store results
                Values.Add(Convert.ToDouble(seed) / M);
            }
        }

        public static void RealVer2(ref List<double>Values)
        {
            //All values are between 1 and 0
            Values.Clear();


            //Double variable types used for both variables and constants
            const double A = 48271.0;
            const double M = 2147483647.0;

            const double Q = M / A;
            const double R = M % A;

            double lo = 0;
            double hi = 0;
            double test = 0;

            double seed = 30102000;

            //100 iterations are used for generation
            for (int i = 1; i <= 100; i++)
            {
                hi = Math.Truncate(seed / Q);
                lo = seed - Q * hi;
                test = A * lo - R * hi;

                //As with Integer Version 2, an if/else clause is used to ensure that the test value can be correctly represented
                if (test > 0.0)
                {
                    seed = test;
                }
                else
                {
                    seed = test + M;
                }

                Values.Add(seed / M);
            }
        }
    }
}
