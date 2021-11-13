using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    public enum OOC {
        joyful,
        distressed,
        hopeful,
        fearful,
        relieve,
        disappointed,
        proud,
        remorseful,
        gratefuk,
        angry

    }
    
    [ComplexType] 
    public class PlayerEmotionalStatus : IValueObject
    {

        public OOC Status { get;  private set; }

        public PlayerEmotionalStatus()
        {
        }

        public PlayerEmotionalStatus(OOC status)
        {
            Status = status;
            
        }

        public void updateEmotionalStatus(OOC status){
            try{
                this.Status = status; // verify
            } catch{
                throw new BusinessRuleValidationException("The provided emotional status is not valid.");
            }
        }
    }
}
