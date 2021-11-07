using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfileName : IValueObject
    {
    
        public string name { get;  private set; }

        public ProfileName(string name)
        {
            this.updateName(name);
            
        }

        public void updateName(string str){
            try{
                this.name = str;
            } catch{
                throw new BusinessRuleValidationException("The provided name is not valid.");
            }
        }
    }
}