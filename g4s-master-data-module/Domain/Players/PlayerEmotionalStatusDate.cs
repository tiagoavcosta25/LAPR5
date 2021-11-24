using System;
using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerEmotionalStatusDate :  IValueObject{

        public string CreationDate{ get; private set;}

        public PlayerEmotionalStatusDate(){
            this.CreationDate = DateTime.Now.ToString();
        }
    }
}