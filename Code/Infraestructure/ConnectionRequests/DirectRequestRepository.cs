using DDDNetCore.Domain.ConnectionRequests;
using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infraestructure.ConnectionRequests
{
    public class DirectRequestRepository : BaseRepository<DirectRequest, ConnectionRequestId>, IDirectRequestRepository
    {
        private readonly DbSet<DirectRequest> _dbdirectRequest;

        public DirectRequestRepository(DDDSample1DbContext context) : base(context.DirectRequests)
        {
            _dbdirectRequest = context.DirectRequests;
        }

        public async Task<List<DirectRequest>> GetAllUserPendingDirectRequestsAsync(PlayerId playerId)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbdirectRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) && x.Target.Equals(playerId))
                .ToListAsync();
        }

        public async Task<DirectRequest> GetPendingDirectRequestByPlayerIds(PlayerId player, PlayerId target)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbdirectRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) &&
                x.Player.Equals(player) && x.Target.Equals(target))
                .FirstOrDefaultAsync();
        }
    }
}
