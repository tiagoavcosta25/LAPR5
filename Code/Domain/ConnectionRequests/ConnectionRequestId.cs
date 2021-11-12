using DDDSample1.Domain.Shared;
using Newtonsoft.Json;
using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.ConnectionRequests
{
    [ComplexType]
    public class ConnectionRequestId : EntityId
    {
        [JsonConstructor]
        public ConnectionRequestId(Guid value) : base(value)
        { 
        }

        public ConnectionRequestId(String value) : base(value)
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
