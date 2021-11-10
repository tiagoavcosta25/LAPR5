using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Connections
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

    }
}
