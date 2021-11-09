using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerPhoneNumber : IValueObject
    {
    
        public double phoneNumber { get;  private set; }

        public PlayerPhoneNumber()
        {
        }

        public PlayerPhoneNumber(double phoneNumber)
        {
            this.updatePhonenNumber(phoneNumber);
            
        }

        public void updatePhonenNumber(double number){
            try{
                this.phoneNumber = number;
            } catch{
                throw new BusinessRuleValidationException("The provided phone number is not valid.");
            }
        }
    }
}