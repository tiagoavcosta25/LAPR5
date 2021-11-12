using DDDSample1.Domain.Shared;
using Newtonsoft.Json;
using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Missions
{
    [ComplexType]
    public class MissionId : EntityId
    {
        [JsonConstructor]
        public MissionId(Guid value) : base(value)
        {
        }

        public MissionId(String value) : base(value)
        { 
        }

        override
        protected Object createFromString(String text)
        {
            return new Guid(text);
        }

        override
        public String AsString()
        {
            Guid obj = (Guid)base.ObjValue;
            return obj.ToString();
        }

        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }
    }
}
