using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Shared
{
    [ComplexType]
    public class Tag : IValueObject
    {
        [Required]
        [MaxLength(50)]
        public string tagName { get; private set; }

        public Tag(string tagName) 
        {
            this.tagName = tagName;
        }
    }
}
