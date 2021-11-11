using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    enum OOC {
        Joyful,
        Distressed,
        Hopeful,
        Fearful,
        Relieve,
        Disappointed,
        Proud,
        Remorseful,
        Gratefuk,
        Angry

    }
    
    [ComplexType] 
    public class PlayerEmotionalStatus : IValueObject
    {

        public string Status { get;  private set; }

        public PlayerEmotionalStatus()
        {
        }

        public PlayerEmotionalStatus(string status)
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
