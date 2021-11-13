using DDDSample1.Domain.Shared;
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Shared
{
    [ComplexType]
    public class ConnectionStrength : IValueObject
    {   
        [Required]
        [Range(1, 100)]
        public int Strength { get; private set; }

        public ConnectionStrength(int strength)
        {
            LocalValidation(strength);
            Strength = strength;
        }

        public void ChangeStrength(int strength)
        {
            LocalValidation(strength);
            Strength = strength;
        }

        private static void LocalValidation(int strength) 
        {
            if (!(strength > 0 && strength <= 100))
                throw new BusinessRuleValidationException("Connection strength cannot be lower than 0 or higher than 100!");
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
