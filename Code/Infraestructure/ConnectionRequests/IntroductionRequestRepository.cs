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
    public class IntroductionRequestRepository : BaseRepository<IntroductionRequest, ConnectionRequestId>, IIntroductionRequestRepository
    {
        private readonly DbSet<IntroductionRequest> _dbintroductionRequest;

        public IntroductionRequestRepository(DDDSample1DbContext context) : base(context.IntroductionRequests)
        {
            _dbintroductionRequest = context.IntroductionRequests;
        }
    }
}
