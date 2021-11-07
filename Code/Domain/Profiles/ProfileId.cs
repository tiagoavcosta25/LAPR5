using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDSample1.Domain.Profiles
{
    [ComplexType]
    public class ProfileId : EntityId
    {
        [JsonConstructor]
        public ProfileId(Guid value) : base(value)
        {
        }

        public ProfileId(String value) : base(value)
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