using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Shared
{
    [ComplexType]
    public class Tag : IValueObject
    {

        public string tagName { get; private set; }

        public Tag(string tagName) 
        {
            this.tagName = tagName;
        }
    }
}
