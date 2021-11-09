using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerLinkedIn : IValueObject
    {
    
        public string Url { get;  private set; }

        public PlayerLinkedIn()
        {
        }

        public PlayerLinkedIn(string linkedin)
        {
            this.updateUrl(linkedin);
            
        }

        public void updateUrl(string linkedin){
            try{
                this.Url = linkedin;
            } catch{
                throw new BusinessRuleValidationException("The provided linkedin Player is not valid.");
            }
        }
    }
}