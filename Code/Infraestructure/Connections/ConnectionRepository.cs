using DDDNetCore.Domain.Connections;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;

namespace DDDNetCore.Infraestructure.Connections
{
    public class ConnectionRepository : BaseRepository<Connection, ConnectionId>, IConnectionRepository
    {
        public ConnectionRepository(DDDSample1DbContext context) : base(context.Connections)
        { 
        }
    }
}
