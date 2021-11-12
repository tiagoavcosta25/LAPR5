using DDDNetCore.Domain.Connections;
using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infraestructure.Connections
{
    public class ConnectionRepository : BaseRepository<Connection, ConnectionId>, IConnectionRepository
    {

        private readonly DbSet<Connection> _dbconnection;

        public ConnectionRepository(DDDSample1DbContext context) : base(context.Connections)
        {
            _dbconnection = context.Connections;
        }

        public async Task<List<Connection>> GetAllUserConnectionsAsync(PlayerId playerId)
        {
            return await _dbconnection
                .Where(x => x.Player.Equals(playerId))
                .ToListAsync();
        }

        public async Task<Connection> GetByBothPlayerIdAsync(PlayerId playerId, PlayerId friendId)
        {
            return await _dbconnection
                .Where(x => x.Player.Equals(playerId) && x.Friend.Equals(friendId))
                .FirstOrDefaultAsync();
        }
    }
}
