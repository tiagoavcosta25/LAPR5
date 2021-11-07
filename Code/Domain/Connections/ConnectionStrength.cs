using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Connections
{
    [ComplexType]
    public class ConnectionStrength : IValueObject
    {

        public int connectionStrength { get; private set; }

        public ConnectionStrength(int strength)
        {
            if (strength >= 0 && strength <= 10)
            {
                this.connectionStrength = strength;
            }
            else
            {
                throw new BusinessRuleValidationException("The strength of the relationship should be between 0 and 10!");
            }
        }
    }
}
