using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Connections
{
    public interface IConnectionRepository : IRepository<Connection, ConnectionId>
    {

        Task<List<Connection>> GetAllUserConnectionsAsync(PlayerId playerId);

        Task<Connection> GetByBothPlayerIdAsync(PlayerId playerId, PlayerId friendId);
    }
}
