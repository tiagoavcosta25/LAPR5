using DDDSample1.Domain.Shared;
using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Shared
{
    [ComplexType]
    public class ConnectionStrength : IValueObject
    {
        public int Strength { get; private set; }

        public ConnectionStrength(int strength)
        {
            Strength = strength;
        }

        public void ChangeStrength(int strength)
        {
                Strength = strength;
        }

        public override bool Equals(object obj)
        {
            return obj is ConnectionStrength strength &&
                   Strength == strength.Strength;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Strength);
        }
    }
}
