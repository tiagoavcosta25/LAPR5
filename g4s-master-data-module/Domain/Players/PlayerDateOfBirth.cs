using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerDateOfBirth : IValueObject
    {
    
        public DateTime date { get;  private set; }

        public PlayerDateOfBirth()
        {
        }

        public PlayerDateOfBirth(int year, int month, int day)
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