using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerPassword : IValueObject
    {
    
        public string password { get;  private set; }

        public PlayerPassword()
        {
        }

        public PlayerPassword(string pswd)
        {
            this.updatePassword(pswd);
            
        }

        public void updatePassword(string pswd){
            try{
                
                this.password = pswd;
            } catch{
                throw new BusinessRuleValidationException("The provided password is not valid.");
            }
        }
    }
}