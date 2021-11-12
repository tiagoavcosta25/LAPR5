using DDDNetCore.Domain.ConnectionRequests;
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
        private readonly DbSet<IntroductionRequest> _dbdirectRequest;

        public DirectRequestRepository(DDDSample1DbContext context) : base(context.DirectRequests)
        {
            _dbdirectRequest = context.IntroductionRequests;
        }
    }
}
