using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profile
{
    [ComplexType]
    public class ProfileEmail : IValueObject
    {
    
        public string address { get;  private set; }

        public ProfileEmail(string email)
        {
            this.updateEmail(email);
            
        }

        public void updateEmail(string email){
            try{
                var temp = new System.Net.Mail.MailAddress(email);
                this.address = temp.Address;
            } catch{
                throw new BusinessRuleValidationException("The provided email address is not valid.");
            }
        }
    }
}