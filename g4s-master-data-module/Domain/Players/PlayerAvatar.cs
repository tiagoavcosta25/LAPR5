using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;
using System.ComponentModel.DataAnnotations;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerAvatar : IValueObject
    {
    
        [Required]
        public string url { get;  private set; }

        public PlayerAvatar()
        {
        }

        public PlayerAvatar(string avatar)
        {
            this.updateAvatar(avatar);
            
        }

        public void updateAvatar(string avatar){
            try{
                
                this.url = avatar;
            } catch{
                throw new BusinessRuleValidationException("The provided Avatar avatar is not valid.");
            }
        }
    }
}