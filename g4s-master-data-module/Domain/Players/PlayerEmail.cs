using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerEmail : IValueObject
    {
    
        public string address { get;  private set; }

        public PlayerEmail()
        {
        }

        public PlayerEmail(string email)
        {
            this.updateEmail(email);
            
        }

        public void updateEmail(string email){
            try{
                
                this.address = email;
            } catch{
                throw new BusinessRuleValidationException("The provided email address is not valid.");
            }
        }
    }
}