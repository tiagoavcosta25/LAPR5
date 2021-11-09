using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Profiles
{
    [ComplexType]
    public class ConnectionStrength : IValueObject
    {

        public int Strength { get; private set; }

        public ConnectionStrength(int strength)
        {
            this.Strength = strength;
        }

        public void ChangeStrength(int strength)
        {
            this.Strength = strength;
        }

    }
}
