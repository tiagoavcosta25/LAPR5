using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerPhoneNumber : IValueObject
    {
    
        public string phoneNumber { get;  private set; }

        public PlayerPhoneNumber()
        {
        }

        public PlayerPhoneNumber(string phoneNumber)
        {
            this.updatePhonenNumber(phoneNumber);
            
        }

        public void updatePhonenNumber(string number){
            try{
                this.phoneNumber = number;
            } catch{
                throw new BusinessRuleValidationException("The provided phone number is not valid.");
            }
        }
    }
}