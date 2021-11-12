using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerName : IValueObject
    {
    
        public string name { get;  private set; }

        public PlayerName()
        {
        }

        public PlayerName(string name)
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