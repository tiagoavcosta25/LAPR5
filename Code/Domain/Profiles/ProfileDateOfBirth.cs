using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profiles
{
    //[ComplexType]
    public class ProfileDateOfBirth : IValueObject
    {
    
        public DateTime date { get;  private set; }

        public ProfileDateOfBirth(int year, int month, int day)
        {
            this.updateDate(year, month, day);
            
        }

        public void updateDate(int year, int month, int day){
            try{
                this.date = new DateTime(year, month, day); 
            } catch{
                throw new BusinessRuleValidationException("The provided date of birth is not valid.");
            }
        }
    }
}