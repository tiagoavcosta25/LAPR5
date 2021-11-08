using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfileFacebook : IValueObject
    {
    
        public string Url { get;  private set; }

        public ProfileFacebook()
        {
        }

        public ProfileFacebook(string facebook)
        {
            this.updateUrl(facebook);
            
        }

        public void updateUrl(string facebook){
            try{
                this.Url = facebook;
            } catch{
                throw new BusinessRuleValidationException("The provided facebook profile is not valid.");
            }
        }
    }
}