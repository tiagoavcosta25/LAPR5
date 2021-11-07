using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfileLinkedIn : IValueObject
    {
    
        public string Url { get;  private set; }

        public ProfileLinkedIn()
        {
        }

        public ProfileLinkedIn(string linkedin)
        {
            this.updateUrl(linkedin);
            
        }

        public void updateUrl(string linkedin){
            try{
                this.Url = linkedin;
            } catch{
                throw new BusinessRuleValidationException("The provided linkedin profile is not valid.");
            }
        }
    }
}