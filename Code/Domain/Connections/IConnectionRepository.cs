using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.Connections
{
    public interface IConnectionRepository : IRepository<Connection, ConnectionId>
    {
    }
}
