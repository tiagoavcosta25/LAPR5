using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Connections
{
    public interface IConnectionRepository : IRepository<Connection, ConnectionId>
    {
    }
}
