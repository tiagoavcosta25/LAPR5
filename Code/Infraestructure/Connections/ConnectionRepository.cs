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

        public async Task<List<PlayerId>> GetFriendsList(PlayerId playerId)
        {
            var friendsList = await _dbconnection
                .Where(c => (c.Player.Equals(playerId)) || (c.Friend.Equals(playerId))).ToListAsync();

            return friendsList.Select(c => !c.Player.Equals(playerId) ? c.Player : c.Friend).ToList();
        }

        public async Task<List<PlayerId>> GetMutualFriends(PlayerId playerId, PlayerId targetId)
        {
            var playerFriendsList = await _dbconnection
                .Where(c => (c.Player.Equals(playerId)) || (c.Friend.Equals(playerId))).ToListAsync();
            var targetFriendsList = await _dbconnection
                .Where(c => (c.Player.Equals(targetId))
                || (c.Friend.Equals(targetId))).ToListAsync();
            List<PlayerId> playerFriends = playerFriendsList.Select(c => !c.Player.Equals(playerId) ? c.Player : c.Friend).ToList();
            List<PlayerId> targetFriends = targetFriendsList.Select(c => !c.Player.Equals(targetId) ? c.Player : c.Friend).ToList();
            return playerFriends.Intersect(targetFriends).ToList();
        }
    }
}
