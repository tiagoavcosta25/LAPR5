using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerFacebook : IValueObject
    {
    
        public string Url { get;  private set; }

        public PlayerFacebook()
        {
        }

        public PlayerFacebook(string facebook)
        {
            this.updateUrl(facebook);
            
        }

        public void updateUrl(string facebook){
            try{
                this.Url = facebook;
            } catch{
                throw new BusinessRuleValidationException("The provided facebook Player is not valid.");
            }
        }
    }
}