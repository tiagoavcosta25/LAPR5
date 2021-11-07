using DDDSample1.Domain.Shared;
using Newtonsoft.Json;
using System;

namespace Code.Domain.Connections
{
    public class ConnectionId : EntityId
    {
        [JsonConstructor]
        public ConnectionId(Guid value) : base(value)
        {
        }

        public ConnectionId(string value) : base(value)
        {
        }

        override
        protected object createFromString(string text)
        {
            return new Guid(text);
        }

        override
        public string AsString()
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