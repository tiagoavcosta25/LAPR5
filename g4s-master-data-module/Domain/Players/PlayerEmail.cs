using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;
using System.ComponentModel.DataAnnotations;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    [Index(nameof(address), IsUnique = true)]
    public class PlayerEmail : IValueObject
    {
    
        [Required]
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