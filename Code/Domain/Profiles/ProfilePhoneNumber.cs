using System;
using System.Data.Metadata.Edm;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfilePhoneNumber : IValueObject
    {
    
        public string phoneNumber { get;  private set; }

        public ProfilePhoneNumber(double phoneNumber)
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