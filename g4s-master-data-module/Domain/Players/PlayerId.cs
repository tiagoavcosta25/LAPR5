using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Players
{
    [ComplexType]
    public class PlayerId : EntityId
    {
        [JsonConstructor]
        public PlayerId(Guid value) : base(value)
        {
        }

        public PlayerId(String value) : base(value)
        {
        }

        override
        protected  Object createFromString(String text){
            return new Guid(text);
        }
        
        override
        public String AsString(){
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        }
    }
}