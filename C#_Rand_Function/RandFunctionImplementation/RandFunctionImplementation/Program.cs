﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Security.Cryptography;

namespace RandFunctionImplementation
{
    class Program
    {
        static void Main(string[] args)
        {
            //The Rand function provided within the C# IDE generates a seed based around the system clock of the PC being used 
            //This can change if the application is a .NET core file, which generates a seed based on a thread specific pseudorandom generator within the system
            //For a closer match to true random, the file format used for this algorithm is a .NET framework
            //In addition, the Random function provided by C# also allows for an inputted seed that will be used in place of the system clock

            //Declares the list to store generated values
            List<double> ReturnValues = new List<double>();
            
            //Implementation 1 of the Random function
            RandImplementation1(ref ReturnValues);

            Console.WriteLine("Implementation 1");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Implementation 2 of the Random function
            RandImplementation2(ref ReturnValues);

            Console.WriteLine("Implementation 2");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Implementation 3 of the Random function

            Console.WriteLine("Implementation 3");

            byte[] ByteValues = RandImplementation3(100);

            for (int i = 0; i <= ByteValues.Count() - 1; i++)
            {
                Console.WriteLine(ByteValues[i]);
            }

            Console.ReadLine();
        }

        static void RandImplementation1(ref List<double>Values)
        {
            //The random class can generate a variety of pseudorandom variables
            //The .next(x) function will generate positive integers between 0 and x

            Values.Clear();

            //By providing no seed value, the rand variable will generate a seed based on the current time
            Random rand = new Random();

            for (int i = 1; i <= 100; i++)
            {
                //Values between 0 and 100 are generated
                Values.Add(rand.Next(100));
            }
        }

        static void RandImplementation2(ref List<double>Values)
        {
            //As with Implementation 1, the .next() function will be used to generate random values
            //However when creating the Random class the control seed 30/10/2000 will be used

            Values.Clear();

            Random rand = new Random(30102000);

            for (int i = 1; i <= 100; i++)
            {
                //Values between 0 and 100 are generated
                Values.Add(rand.Next(100));
            }
        }

        static byte[] RandImplementation3(int size)
        {
            //In additon to Random, C# also provides the RandomNumberGenerator function
            //Unlike the default Random, RandomNumberGenerator is a cryptographic generator
            //To access this generator the System.Security.Crytography library is required

            using (var generator = RandomNumberGenerator.Create())
            {
                //The array values stores the generated bytes created by RandomNumberGenerator
                var values = new byte[size];

                generator.GetBytes(values);

                return values;
            }
        }
    }
}
