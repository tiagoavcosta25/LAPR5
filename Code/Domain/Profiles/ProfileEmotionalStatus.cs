using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfileEmotionalStatus : IValueObject
    {

        public string Status { get;  private set; }

        public ProfileEmotionalStatus()
        {
        }

        public ProfileEmotionalStatus(string status)
        {
            this.updateEmotionalStatus(status);
            
        }

        public void updateEmotionalStatus(string status){
            try{
                this.Status = status; // verify
            } catch{
                throw new BusinessRuleValidationException("The provided emotional status is not valid.");
            }
        }
    }
}